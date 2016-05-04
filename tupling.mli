open Syntax

val trans : typed_term -> typed_term * ((Syntax.id -> Ref_type.t) -> Syntax.id -> Ref_type.t)
