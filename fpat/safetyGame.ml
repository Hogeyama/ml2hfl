open Util
open Combinator

(** Safety games over infinite arenas *)

type t = {
  arena: InfGameArena.t
}
