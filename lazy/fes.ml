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

let coeffs (FES(xttys, ts)) =
  Util.concat_map
    (fun (x, t, _) -> (if Var.is_coeff x then [x] else []) @ coeffs t)
    xttys @
  Util.concat_map coeffs ts

let simplify (FES(xttys, ts)) =
  let ts = Formula.conjuncts (Formula.simplify (Formula.band ts)) in
  if ts = [Formula.tfalse] then
    make [] ts
  else
    make (List.map (fun (x, t, ty) -> x, LinArith.simplify t, ty) xttys) ts


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

(*				eqelim p (equantify p (make [] ts))*)
