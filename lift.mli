(** [lift t] returns lambda-lifting of [t].
    the definitions of let expressions must be side-effect free *)
val lift :
  ?args:Syntax.id list -> Syntax.typed_term ->
  ((Syntax.typ Id.t * (Syntax.id list * Syntax.typed_term)) list * Syntax.typed_term) *
  (Syntax.typ Id.t -> Ref_type.t -> Ref_type.t)
