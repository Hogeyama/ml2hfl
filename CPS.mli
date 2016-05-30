
(** CPS-transformation for source language *)

val trans : Syntax.typed_term -> Syntax.typed_term * ((Syntax.id -> Ref_type.t) -> Syntax.id -> Ref_type.t)
val trans_as_direct : Syntax.typed_term -> Syntax.typed_term * ((Syntax.id -> Ref_type.t) -> Syntax.id -> Ref_type.t)
val trans_typ : Syntax.typ -> Syntax.typ
val trans_ref_typ : Ref_type.t -> Ref_type.t
val trans_ref_typ_as_direct : Ref_type.t -> Ref_type.t
val uncps_ref_type : Ref_type.t -> Ref_type.t -> Ref_type.t
