open Syntax

val typ : Format.formatter -> typ -> unit
val id : Format.formatter -> id -> unit
val id_typ : Format.formatter -> id -> unit
val pattern : Format.formatter -> pattern -> unit
val const : Format.formatter -> const -> unit
val desc : Format.formatter -> desc -> unit
val term : Format.formatter -> term -> unit
val term' : Format.formatter -> term -> unit
val term_typ : Format.formatter -> term -> unit
val defs : Format.formatter -> (id * (id list * term)) list -> unit
val constr : Format.formatter -> term -> unit
val attr : Format.formatter -> attr list -> unit

val string_of_const : const -> string
val string_of_binop : binop -> string
val string_of_typ : typ -> string
