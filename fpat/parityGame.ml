open Util
open Combinator

(* Parity games over infinite arenas *)

type t = {
  arena: InfGameArena.t;
  parity: (Formula.t * int) list
}
