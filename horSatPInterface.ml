open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

exception UnknownOutput

type state = string
type symbol = string
type priority = state * int

type label =
  | Event of string
  | Br_A
  | Br_E
  | Call
  | End

let string_of_label = function
  | Event x -> "event_"^x
  | Br_A -> "br_forall"
  | Br_E -> "br_exists"
  | Call -> "call"
  | End  -> "unit"

type formula =
  | Tt | Ff
  | Label of int * symbol
  | And of formula * formula
  | Or  of formula * formula

type transition = state * symbol * formula
type spec = transition list * priority list

type result =
  | Safe | Unsafe

let version () = "dummy"

let check _ = Safe

let make_apt events (a, b) =
  let q0, q1, q2 = "q0", "q1", "q2" in
  let rec trans = function
    | Event x -> Label (1, if x = a then q1 else q2)
    | Br_A -> And (Label (1, q0), Label (2, q0))
    | Br_E -> Or  (Label (1, q0), Label (2, q0))
    | Call -> Label (1, q0)
    | End  -> Ff
  in
  let syms  = events @ [Event a; Event b; Br_A; Br_E; Call; End] in
  let states = [q0; q1; q2] in
  let omega = List.sort [(q0, 0); (q1, 1); (q2, 2)] in
  let delta =
    List.map
      (fun state ->
        List.map
          (fun sym ->
            (state, string_of_label sym, trans sym))
          syms
      )
      states in
  let delta' = List.sort (List.flatten delta) in
  delta', omega

let make_fair_nonterm_spec events streett : spec =
  assert (List.length streett = 1);
  let (a, b) = List.hd streett in
  let es = List.map (fun e -> Event e) (List.filter (fun e -> e != a && e != b) events) in
  make_apt es (a, b)
