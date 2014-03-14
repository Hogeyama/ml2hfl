type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
type path = int list

val root : 'a tree -> 'a
val flatten : 'a tree -> 'a list
val map : (path -> 'a -> 'b) -> 'a tree -> 'b tree
val fold : ('a -> 'a -> 'a) -> ('b -> 'a) -> 'b tree -> 'a
val proj : path -> 'a tree -> 'a tree
val update : path -> 'a tree -> 'a tree -> 'a tree

val for_all : ('a -> bool) -> 'a tree -> bool
val exists : ('a -> bool) -> 'a tree -> bool

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a tree -> unit
