open Syntax

val trans : typed_term -> typed_term * (Syntax.id -> Ref_type.t -> Ref_type.t)
