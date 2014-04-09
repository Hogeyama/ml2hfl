
type color =
    Blue
  | Red
  | Green
  | Cyan

val init : unit -> unit
val set : Format.formatter -> color -> unit
val reset : Format.formatter -> unit

val fprintf : Format.formatter -> color -> ('a, Format.formatter, unit) format -> 'a
val printf : color -> ('a, Format.formatter, unit) format -> 'a

val blue : Format.formatter -> string -> unit
val red : Format.formatter -> string -> unit
val green : Format.formatter -> string -> unit
val cyan : Format.formatter -> string -> unit
