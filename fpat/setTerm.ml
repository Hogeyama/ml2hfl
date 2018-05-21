open Util
open Combinator
open Term

(** Set term expressions *)

(** {6 Auxiliary constructors} *)

let mk_empty ty = mk_const (Const.SEmpty ty)
let mk_add ty e s = mk_app (mk_const (Const.SAdd ty)) [e; s]
let mk_union ty s1 s2 = mk_app (mk_const (Const.SUnion ty)) [s1; s2]
let mk_intersect ty s1 s2 = mk_app (mk_const (Const.SIntersect ty)) [s1; s2]
let mk_diff ty s1 s2 = mk_app (mk_const (Const.SDiff ty)) [s1; s2]
let mk_comp ty s = mk_app (mk_const (Const.SComplement ty)) [s]
