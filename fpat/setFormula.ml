open Util
open Combinator

(** Formulas on sets *)

(** {6 Auxiliary constructors} *)

let mk_mem ty e s = SetAtom.mk_mem ty e s |> Formula.of_atom
let mk_subset ty s1 s2 = SetAtom.mk_subset ty s1 s2 |> Formula.of_atom
