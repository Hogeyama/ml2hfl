(** Predicates *)

type t = Var.t * (Term.t * SimType.t) list

let pr ppf (pid, ttys) =
  Format.fprintf ppf "P[%a](%a)" Var.pr pid (Util.pr_list Term.pr ",") (List.map fst ttys)

let make pid ttys = pid, ttys

let fvs (_, ttys) = Util.concat_map (fun (t, _) -> Term.fvs t) ttys
let coeffs (_, ttys) = Util.concat_map (fun (t, _) -> Term.coeffs t) ttys

let simplify (pid, ttys) = pid, List.map (fun (t, ty) -> LinArith.simplify t, ty) ttys

let subst sub (pid, ttys) =
  pid, List.map (fun (t, ty) -> Term.subst sub t, ty) ttys

let subst_fixed sub (pid, ttys) =
  pid, List.map (fun (t, ty) -> Term.subst_fixed sub t, ty) ttys

let of_pid env pid =
  pid, List.map (fun (t, ty) -> Term.make_var t, ty) (RefType.visible_vars env pid)

let of_pid_vars env pid =
  pid, RefType.visible_vars env pid

let equiv env (pid1, ttys1) (pid2, ttys2) =
  pid1 = pid2 &&
  Cvc3Interface.implies
    env
    (List.map2
      (fun (t1, ty1) (t2, ty2) ->
        let _ = if !Global.debug then assert (ty1 = ty2) in
        Formula.simplify (Formula.eq_ty ty1 t1 t2))
      ttys1 ttys2)

let matches p env (pid1, ttys1) (pid2, ttys2) =
  pid1 = pid2 &&
  List.for_all2
    (fun (t1, ty1) (t2, ty2) ->
      let _ = if !Global.debug then assert (ty1 = ty2) in
      t1 = t2 ||
      List.exists p (Term.fvs t2) ||
      (List.for_all (fun x -> not (p x)) (Term.fvs t1) &&
      Cvc3Interface.implies env [Formula.simplify (Formula.eq_ty ty1 t1 t2)]))
    ttys1 ttys2
