open Util
open Combinator

(** Recurrence games over infinite arenas *)

type t = {
  arena: InfGameArena.t;
  recurrence: Formula.t
}
