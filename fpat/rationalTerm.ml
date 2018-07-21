open Util

include Term

(** {6 Auxiliary constructors} *)

let make n1 n2 = mk_const (Const.Rational(n1, n2))
