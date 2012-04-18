open ExtList
open Term

(** Formulas with explicit substitutions *)

(** each element (x, t, ty) of the first has functional dependencies x -> t and x -> ty*)
type t = FES of Tsubst.t * Term.t list

(** {6 Functions on formulas with explicit substitutions} *)

let make xttys ts = FES(xttys, ts)

(** ignore equalities on functions *)
let formula_of (FES(xttys, ts)) = Formula.band (Tsubst.formula_of xttys :: ts)

let band fess =
  let rec aux fess =
    match fess with
      [] -> [], []
    | (FES(xttys, ts))::fess' ->
        let xttys', ts' = aux fess' in
        xttys @ xttys', ts @ ts'
  in
  let xttys, ts = aux fess in
  let ts = Formula.simplify_conjuncts ts in
  FES(xttys, ts)

let simplify (FES(xttys, ts)) =
  let ts = Formula.conjuncts (Formula.simplify (Formula.band ts)) in
  if ts = [Formula.tfalse] then
    make [] ts
  else
    make (List.map (fun (x, t, ty) -> x, LinArith.simplify t, ty) xttys) ts

let elim_duplicate (*xs*) (FES(xttys, ts1)) =
  let xttys, ts2 = Tsubst.elim_duplicate xttys in
  make xttys (ts1 @ ts2)

let rec nlfvs t =
  match fun_args t with
    Var(_, _), [] -> []
  | Const(_, Const.Mul), [_; _] ->
      (try
        let _ = LinArith.of_term t in
        []
      with Invalid_argument _ ->
        fvs t(*???*))
  | Const(_, Const.Div), [t1; t2] | Const(_, Const.Mod), [t1; t2] ->
      assert false
  | Const(_, c), ts ->
      Util.concat_map nlfvs ts
  | Call(_, _, _), [] | Ret(_, _, _, _), [] | Error(_), [] -> assert false
  | Forall(_, env, t), [] | Exists(_, env, t), [] -> Util.diff (nlfvs t) (List.map fst env)

let pr ppf fes =
  if true then
    let FES(xttys, ts) = fes in
    let _ = Format.fprintf ppf "%a" Tsubst.pr xttys in
				let _ =
		    if xttys <> [] then
  		    Format.fprintf ppf " && "
    in
    Format.fprintf ppf "%a" (Util.pr_list pr " && ") ts
  else if true then
    let FES(xttys, ts) = fes in
    let ts = Formula.conjuncts (Tsubst.formula_of xttys) @ ts in
    Format.fprintf ppf "%a" (Util.pr_list pr " && ") ts
  else
    Format.fprintf ppf "%a" pr (formula_of fes)


let equantify p (FES(xttys, ts) as fes) =
  let _ = Global.log_begin "equantify" in
  let _ = Global.log (fun () -> Format.printf "input: @[<v>%a@]@," pr fes) in
  let nlfvs = nlfvs (formula_of (FES(xttys, ts))) in
  let rec aux ts xttys0 ts0 =
    match ts with
      [] -> xttys0, ts0
    | t::ts' ->
        let sub =
          let dom = List.map Util.fst3 xttys0 in
          let sub = Formula.sub_of p dom t in
          List.filter (fun (x, _, _) -> not (List.mem x nlfvs) (*|| t is constant*)) sub
        in
        let xttys0, ts0 =
          match sub with
            [] -> xttys0, t::ts0
          | [xtty] ->
              xtty::xttys0, ts0
        in
        aux ts' xttys0 ts0
  in
  let xttys0, ts0 = aux ts xttys [] in
  let fes = elim_duplicate (make xttys0 ts0) in
  let _ = Global.log (fun () -> Format.printf "output: @[<v>%a@]" pr fes) in
  let _ = Global.log_end "equantify" in
  fes


let subst sub (FES(xttys, ts)) =
  make
    (List.map (fun (x, t, ty) -> subst_var sub x, Term.subst sub t, ty) xttys)
    (List.map (subst sub) ts)

let subst_fixed sub (FES(xttys, ts)) =
  make
    (List.map (fun (x, t, ty) -> subst_fixed_var sub x, Term.subst_fixed sub t, ty) xttys)
    (List.map (Formula.subst_fixed sub) ts)


let fvs (FES(xttys, ts)) = Tsubst.fvs xttys @ Util.concat_map fvs ts

let coefficients (FES(xttys, ts)) =
  Util.concat_map
    (fun (x, t, _) -> (if Var.is_coeff x then [x] else []) @ coefficients t)
    xttys @
  Util.concat_map coefficients ts



(** apply explicit substitutions to variables x not satifying p
    ignore equalities on functions *)
let eqelim p (FES(xttys, ts) as fes) =
  if xttys = [] then
		  let _ = Global.log (fun () -> Format.printf "skipping eqelim@,") in
    fes
  else
		  let _ = Global.log_begin "eqelim" in
		  let _ = Global.log (fun () -> Format.printf "input: @[<v>%a@]@," pr fes) in
		  let xttys1, xttys2 = List.partition (fun (x, _, _) -> p x) xttys in
		  let _ = Global.log (fun () -> Format.printf "sub: %a@," Tsubst.pr xttys2) in
		  let sub x = List.assoc x (List.map (fun (x, t, _) -> x, t) xttys2) in
		  let ts = List.map (Formula.subst_fixed sub) ts in
		  let xttys1 = List.map (fun (x, t, ty) -> x, Term.subst_fixed sub t, ty) xttys1 in
		  let res = FES(xttys1, ts) in
		  let _ = Global.log (fun () -> Format.printf "output: @[<v>%a@]" pr res) in
		  let _ = Global.log_end "eqelim" in
		  res
(*
  let rec aux xttys (xttys1, xttys2) =
    match xttys with
      [] ->
        (xttys1, xttys2)
    | (x, t, ty)::xttys ->
        (match t with
          Var(_, y)
            when not (p y) &&
                 not (List.exists (fun (x, _, _ ) -> Var.equiv y x) xttys2) ->
            aux xttys (xttys1, (y, make_var x, ty)::xttys2)
        | _ ->
            aux xttys ((x, t, ty)::xttys1, xttys2))
  in
  let xttys11, xttys12 = aux xttys1 ([], []) in
  let sub x = List.assoc x (List.map (fun (x, t, _) -> x, t) xttys12) in
  let ts = List.map (Formula.subst_fixed sub) (ts @ List.map Tsubst.formula_of_elem xttys11) in
  let ts = Util.concat_map Formula.conjuncts ts in
  band ts
*)
