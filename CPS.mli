
(** CPS-transformation for source language *)

val trans : Syntax.typed_term -> Syntax.typed_term * ((Syntax.id -> Ref_type.t) -> Syntax.id -> Ref_type.t)
