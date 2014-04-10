
type color =
    Default
  | Bright
  | Dim
  | Underscore
  | Blink
  | Reverse
  | Hidden
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | BG_Black
  | BG_Red
  | BG_Green
  | BG_Yellow
  | BG_Blue
  | BG_Magenta
  | BG_Cyan
  | BG_White

val init : unit -> unit
val set : Format.formatter -> color -> unit
val reset : Format.formatter -> unit

val fprintf : Format.formatter -> color -> ('a, Format.formatter, unit) format -> 'a
val printf : color -> ('a, Format.formatter, unit) format -> 'a

val blue : Format.formatter -> string -> unit
val red : Format.formatter -> string -> unit
val green : Format.formatter -> string -> unit
val cyan : Format.formatter -> string -> unit
