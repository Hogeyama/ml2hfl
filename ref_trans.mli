open Syntax

val trans : (id -> typ) -> typed_term -> typed_term
val make_fun_tuple : typed_term -> typed_term
