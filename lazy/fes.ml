open ExtList
open Term

(** Formulas with explicit substitutions *)

(** each element (x, t, ty) has functional dependencies x -> t and x -> ty *)
type t = FES of Tsubst.t * Term.t list

(** {6 Functions on formulas with explicit substitutions} *)

let make xttys ts = FES(xttys, ts)

(** ignore equalities on functions *)
let formula_of (FES(xttys, ts)) = Formula.band (Tsubst.formula_of xttys :: ts)

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

let band fess =
  let rec aux fess =
    match fess with
      [] -> [], []
    | (FES(xttys, ts))::fess' ->
        let xttys', ts' = aux fess' in
        xttys @ xttys', ts @ ts'
  in
  let xttys, ts = aux fess in
  FES(xttys, Formula.simplify_conjuncts ts)

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

let simplify (FES(xttys, ts)) =
  let ts = Formula.conjuncts (Formula.simplify (Formula.band ts)) in
  if ts = [Formula.tfalse] then
    make [] ts
  else
    make (List.map (fun (x, t, ty) -> x, LinArith.simplify t, ty) xttys) ts

let elim_duplicate (FES(xttys, ts1)) =
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
        Term.fvs t(*???*))
  | Const(_, Const.Div), [t1; t2] | Const(_, Const.Mod), [t1; t2] ->
      assert false
  | Const(_, c), ts ->
      Util.concat_map nlfvs ts
  | Call(_, _, _), [] | Ret(_, _, _, _), [] | Error(_), [] -> assert false
  | Forall(_, env, t), [] | Exists(_, env, t), [] -> Util.diff (nlfvs t) (List.map fst env)


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
		  let _ = Global.log (fun () -> Format.printf "substitution: %a@," Tsubst.pr xttys2) in
    let sub = Tsubst.fun_of xttys2 in
		  let ts = List.map (Formula.subst_fixed sub) ts in
		  let xttys1 = List.map (fun (x, t, ty) -> x, Term.subst_fixed sub t, ty) xttys1 in
		  let fes = FES(xttys1, ts) in
		  let _ = Global.log (fun () -> Format.printf "output: @[<v>%a@]" pr fes) in
		  let _ = Global.log_end "eqelim" in
		  fes

let eqelim_conjuncts pid p fes =
  let ts = Formula.conjuncts (formula_of fes) in
  if true then
		  let eqcs, ts =
		    Util.partition_map
		      (fun t ->
		        match fun_args t with
		          Const(_, Const.EqInt), [Var(_, x1); Var(_, x2)] ->
              `L([x1; x2])
		        | _ -> `R(t))
		      ts
		  in
		  let eqcs =
				  let rec aux eqcs1 eqcs2 =
				    match eqcs2 with
				      [] -> eqcs1
				    | eqc2::eqcs2' ->
				        let eqcs1' =
				  								let flag = ref false in
		            let eqcs = List.map (fun eqc1 -> if Util.inter eqc2 eqc1 <> [] then let _ = flag := true in List.unique (eqc1 @ eqc2) else eqc1) eqcs1 in
				          if !flag then eqcs else eqc2 :: eqcs
				        in
				        aux eqcs1' eqcs2'
				  in
      Util.fixed_point (aux []) (fun eqcs1 eqcs2 -> List.length eqcs1 = List.length eqcs2) eqcs
    in
    let ts', sub =
      Util.flatten_split
		      (List.map
		        (fun eqc ->
		          let xs1, xs2 = List.partition p eqc in
		          match xs1 with
		            [] ->
		              [], List.map (fun x -> x, Term.make_var (List.hd xs2), SimType.Int) (List.tl xs2)
		          | x::xs ->
		              let x, xs = if List.mem pid xs1 then pid, Util.diff xs1 [pid] else x, xs in
		              List.map (fun x' -> Formula.eqInt (Term.make_var x) (Term.make_var x')) xs,
		              List.map (fun x' -> x', Term.make_var x, SimType.Int) xs2)
		        eqcs)
    in
    let ts = List.map (Term.subst (Tsubst.fun_of sub)) ts @ ts' in
    make [] ((*Formula.simplify_conjuncts*) ts)
  else
				eqelim p (equantify p (make [] ts))
