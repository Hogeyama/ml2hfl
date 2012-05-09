(** Predicates *)

type t = Var.t * Term.t list

let pr ppf (pid, ts) =
  Format.fprintf ppf "P[%a](%a)" Var.pr pid (Util.pr_list Term.pr ",") ts

let make pid xs = pid, List.map Term.make_var xs

let fvs (_, ts) = Util.concat_map Term.fvs ts
let coeffs (_, ts) = Util.concat_map Term.coeffs ts

let simplify (pid, ts) = pid, List.map LinArith.simplify ts

let subst sub (pid, ts) =
  (pid, List.map (Term.subst sub) ts)

let subst_fixed sub (pid, ts) =
  (pid, List.map (Term.subst_fixed sub) ts)

let of_pid env pid =
  pid, List.map Term.make_var (RefType.visible_vars env pid)

let of_pid_vars env pid =
  pid, RefType.visible_vars env pid
