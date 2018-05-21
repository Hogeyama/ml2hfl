open Util
open Combinator

(** Literals related to ADTs *)

(** {6 Auxiliary constructors} *)

let mk_recognizer ty x t = ADTAtom.mk_recognizer ty x t |> Literal.of_atom
