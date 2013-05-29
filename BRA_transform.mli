(***** Constants *****)

val hole_term : Syntax.typed_term

(***** Functions *****)

val everywhere_expr : (Syntax.typed_term -> Syntax.typed_term) -> Syntax.typed_term -> Syntax.typed_term

val regularization : Syntax.typed_term -> Syntax.typed_term

val show_typed_term : Syntax.typed_term -> string

val retyping : Syntax.typed_term -> (Parsetree.toplevel_phrase list * Syntax.typed_term)

val extract_functions : Syntax.typed_term -> BRA_types.function_info list

val extract_id : Syntax.typed_term -> Syntax.id

val to_holed_programs : Syntax.typed_term (* target program *) -> BRA_types.function_info list (* defined functions *) -> BRA_types.holed_program list (* holed transformed program *)

(* construct linear lexicographic ranking function *)
val construct_LLRF : BRA_types.predicate_info -> Syntax.typed_term

(* plug holed program with predicate *)
val pluging : BRA_types.holed_program -> Syntax.typed_term -> Syntax.typed_term
