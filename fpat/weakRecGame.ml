open Util
open Combinator

(** Weak recurrence games over infinite game arenas *)

type t = {
  arena: InfGameArena.t;
  components: Formula.t list;
  recurrence: Formula.t
}
