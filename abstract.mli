
(** Predicate abstraction *)

val abstract : Syntax.t -> Syntax.t
(** [abstract t] で [t] の述語抽象を求める．
    [t] は CPS かつ型付きでなければならない．
*)

val abstract_mutable : Syntax.t -> Syntax.t

val abst_ext_funs : Syntax.t -> Syntax.t
