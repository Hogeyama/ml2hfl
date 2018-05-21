open Term

(** Tuple term expressions *)

(** {6 Auxiliary constructors} *)

val make : Type.t list -> t list -> t
val mk_proj : Type.t list -> int -> t -> t
val mk_tuple2 : t list -> t

(** {6 Auxiliary destructors} *)

val let_tuple : t -> (Type.t list -> t list -> 'a) -> (unit -> 'a) -> 'a
val is_tuple : t -> bool

val elements_of : t * Type.t -> (t * Type.t) list
val get_proj_var : t -> TypEnv.t
val has_proj : t -> bool
val eval_proj : t -> t
