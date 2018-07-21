(** Vector term expressions *)
(** @todo elim FDup *)

type t = Term.t

(** {6 Auxiliary constructors} *)

val make : Type.t -> t list -> t
val make_fvec : t list -> t

(** {6 Operators} *)

val add : int -> t -> t -> t
val sub : int -> t -> t -> t
val mul : int -> t -> t -> t
val max : int -> t -> t -> t
val min : int -> t -> t -> t
(*val dup : int -> t -> t*)
val rnorm : int -> t -> t
val normalize : int -> t -> t
val dot : int -> t -> t -> t
val elem : int -> t -> int -> t

val simplify : t -> t

val vshuffle : int -> t -> t -> t -> t
val vinsert_element : int -> t -> t -> t -> t
val vextract_element : int -> t -> t -> t

val sum : t -> t

(** {6 Inspectors} *)

val is_const : t -> bool
