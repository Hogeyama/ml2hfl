open Syntax

val mutable_record : typed_term -> typed_term
val list : typed_term -> typed_term * ((Syntax.id -> Ref_type.t) -> Syntax.id -> Ref_type.t)
val recdata : typed_term -> typed_term
val array : typed_term -> typed_term

val abst_ref : typed_term -> typed_term
