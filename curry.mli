val remove_pair : ?check:bool -> Syntax.typed_term -> Syntax.typed_term * ((Syntax.id -> Ref_type.t) -> Syntax.id -> Ref_type.t)
val remove_pair_direct : Syntax.typed_term -> Syntax.typed_term * ((Syntax.id -> Ref_type.t) -> Syntax.id -> Ref_type.t)
