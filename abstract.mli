
(** Abstraction for data types *)

val abstract_mutable : Syntax.typed_term -> Syntax.typed_term

(*
val abst_ext_funs : Syntax.typed_term -> Syntax.typed_term
*)

val abstract_recdata : Syntax.typed_term -> Syntax.typed_term
val abstract_list : Syntax.typed_term -> Syntax.typed_term
val abst_ext_funs : Syntax.typed_term -> Syntax.typed_term


         val get_match_bind_cond :
           Syntax.typed_term ->
           Syntax.typed_pattern ->
           (Syntax.typed_term Type.t Id.t * Syntax.typed_term) list *
           Syntax.typed_term
