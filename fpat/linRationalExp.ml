open Util
open Combinator

(** Linear expressions with rational coefficients *)

include LinExp.Make(Coeff.CoeffRat)
