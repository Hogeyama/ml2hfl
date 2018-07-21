open Util
open Combinator

(** Atoms on sets *)

(** {6 Auxiliary constructors} *)

let mk_mem ty e s =
  Term.mk_app (Term.mk_const (Const.SMem ty)) [e; s] |> Atom.of_term
let mk_subset ty s1 s2 =
  Term.mk_app (Term.mk_const (Const.SSubset ty)) [s1; s2] |> Atom.of_term
