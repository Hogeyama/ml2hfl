open Syntax

(** Encode mutable record as record with references *)
val mutable_record : Program.t -> Program.t

(** Encode record as tuple *)
val record : Program.t -> Program.t

(** Encode simple variant as integer *)
val simple_variant : Program.t -> Program.t

(** Encode list as function *)
val list : Program.t -> Program.t * ((Syntax.id -> Ref_type.t) -> Syntax.id -> Ref_type.t)

(** Encode recursive data as function *)
val recdata : Program.t -> Program.t

(** Encode recursive data as function with reference *)
val array : Program.t -> Program.t

(** Abstract away content of reference *)
val abst_ref : Program.t -> Program.t

val all : Program.t -> Program.t

val typ_of : (Program.t -> Program.t) -> typ -> typ

val abst_rec_record : Program.t -> Program.t

val abst_poly_comp : Program.t -> Program.t
