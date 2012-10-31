
(** Abstraction for data types *)

val abstract_mutable : Syntax.typed_term -> Syntax.typed_term

(*
  val abst_ext_funs : Syntax.typed_term -> Syntax.typed_term
*)

val abstract_recdata : Syntax.typed_term -> Syntax.typed_term
val abstract_list : Syntax.typed_term -> Syntax.typed_term * (Syntax.id -> Ref_type.t -> Ref_type.t)
