open Util
open Combinator

(** Liveness games over infinite arenas *)

type t = {
  arena: InfGameArena.t
}
