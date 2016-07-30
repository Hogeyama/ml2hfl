open Syntax

(** Encode mutable record as record with references *)
val mutable_record : typed_term -> typed_term

(** Encode record as tuple *)
val record : typed_term -> typed_term

(** Encode list as function *)
val list : typed_term -> typed_term * ((Syntax.id -> Ref_type.t) -> Syntax.id -> Ref_type.t)

(** Encode recursive data as function *)
val recdata : typed_term -> typed_term

(** Encode recursive data as function with reference *)
val array : typed_term -> typed_term

(** Abstract away content of reference *)
val abst_ref : typed_term -> typed_term
