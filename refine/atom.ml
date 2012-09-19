open ExtList
open Util

(** Atoms *)

(** {6 The type of atoms} *)

type t = Var.t * (Term.t * SimType.t) list

(** {6 Printers} *)

let pr ppf (pid, ttys) =
  Format.fprintf ppf "P[%a](%a)" Var.pr pid (Util.pr_list TypTerm.pr ",") ttys

(** {6 Constructors} *)

let make pid ttys = pid, ttys

let of_pred (pid, xtys) = make pid (List.map (fun (x, ty) -> Term.make_var x, ty) xtys)

(** {6 Basic functions} *)

let fvs (_, ttys) = Util.concat_map (fun (t, _) -> Term.fvs t) ttys

let coeffs (_, ttys) = Util.concat_map (fun (t, _) -> Term.coeffs t) ttys

(** @return the number of duplicate predicate variables *)
let num_dup atms =
  let atmss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) atms in
  Util.sum (List.map (fun atms -> List.length atms - 1) atmss)

let simplify (pid, ttys) = pid, List.map (fun (t, ty) -> LinArith.simplify t, ty) ttys

let subst sub (pid, ttys) =
  pid, List.map (fun (t, ty) -> Term.subst sub t, ty) ttys

let subst_fixed sub (pid, ttys) =
  pid, List.map (fun (t, ty) -> Term.subst_fixed sub t, ty) ttys

let equiv env (pid1, ttys1) (pid2, ttys2) =
  pid1 = pid2 &&
  Cvc3Interface.implies env (List.map2 (FormulaUtil.simplify -|| Formula.eq_tty) ttys1 ttys2)

(** @return whether there is a substitution sub for variables {x | p x} such that
            equiv env (subst sub (pid2, ttys2)) (pid1, ttys1) *)
let matches p env (pid1, ttys1) (pid2, ttys2) =
  pid1 = pid2 &&
  List.for_all2
    (fun tty1 tty2 ->
      TypTerm.equiv tty1 tty2 ||
      List.exists p (TypTerm.fvs tty2) (* @todo *) ||
      (List.for_all (fun x -> not (p x)) (TypTerm.fvs tty1) (* @todo *) &&
       Cvc3Interface.implies env [(FormulaUtil.simplify -|| Formula.eq_tty) tty1 tty2]))
    ttys1 ttys2
