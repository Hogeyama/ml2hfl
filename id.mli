type 'a t = {id:int; name:string; typ:'a}

val new_int : unit -> int
val get_counter : unit -> int
val set_counter : int -> unit
val save_counter : unit -> unit
val reset_counter : unit -> unit
val clear_counter : unit -> unit

val make : int -> string -> 'a -> 'a t
val new_var : string -> 'a -> 'a t
val new_var_id : 'a t -> 'a t

val id : 'a t -> int
val name : 'a t -> string
val typ : 'a t -> 'a

val to_string : 'a t -> string
val from_string : string -> 'a -> 'a t

val compare : 'a t -> 'a t -> int
val same : 'a t -> 'a t -> bool

val set_name : 'a t -> string -> 'a t
val set_typ : 'a t -> 'a -> 'a t
val add_name : 'a t -> string -> 'a t

val assoc : 'a t -> ('b t * 'c) list -> 'c
val mem : 'a t -> 'b t list -> bool
val mem_assoc : 'a t -> ('b t * 'c) list -> bool

val print : Format.formatter -> 'a t -> unit
