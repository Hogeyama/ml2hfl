open Util
open Combinator

(* Infinite game arenas for higher-order functional programs *)

(* @require angel never calls demon and vice versa
   @require angel and demon have the type state -> state *)
type t = {
  fdefs: Fdef.t list;
  types: TypEnv.t;
  angel: string;
  demon: string
}
