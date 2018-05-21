open Util
open Combinator

(** Weak parity games over infinite game arenas *)

type t = {
  arena: InfGameArena.t;
  components: Formula.t list;
  parity: (Formula.t * int) list
}
