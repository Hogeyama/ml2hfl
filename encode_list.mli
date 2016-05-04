
(** Encoding for lists *)

val trans : Syntax.typed_term -> Syntax.typed_term * ((Syntax.id -> Ref_type.t) -> Syntax.id -> Ref_type.t)
