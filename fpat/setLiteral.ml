open Util
open Combinator

(** Literals on sets *)

(** {6 Auxiliary constructors} *)

let mk_mem ty e s = SetAtom.mk_mem ty e s |> Literal.of_atom
let mk_subset ty s1 s2 = SetAtom.mk_subset ty s1 s2 |> Literal.of_atom
          
