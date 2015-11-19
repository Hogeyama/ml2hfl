type 'a t = Node of 'a * 'a t list
type path = int list
val leaf : 'a -> 'a t
val root : 'a t -> 'a
val flatten : 'a t -> 'a list
val map : (int list -> 'a -> 'b) -> 'a t -> 'b t
val fold : ('a -> 'b list -> 'b) -> 'a t -> 'b
val for_all : ('a -> bool) -> 'a t -> bool
val exists : ('a -> bool) -> 'a t -> bool
val proj : int list -> 'a t -> 'a t
val print :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val update : int list -> 'a t -> 'a t -> 'a t
val zip : 'a t -> 'b t -> ('a * 'b) t
