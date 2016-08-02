
open Syntax

(** CPS-transformation for source language *)

val trans : typed_term -> typed_term * ((id -> Ref_type.t) -> id -> Ref_type.t)
val trans_as_direct : typed_term -> typed_term * ((id -> Ref_type.t) -> id -> Ref_type.t)
val trans_typ : typ -> typ -> typ
val trans_ref_typ : Ref_type.t -> Ref_type.t
val trans_ref_typ_as_direct : Ref_type.t -> Ref_type.t
val trans_no_main : typed_term ->  typed_term
