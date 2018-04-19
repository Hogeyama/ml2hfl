type mode = Default | Allow_recursive | Use_empty_pred

exception Ref_type_not_found

val check : ?mode:mode -> Ref_type.Env.t -> Syntax.term -> Ref_type.t -> bool
