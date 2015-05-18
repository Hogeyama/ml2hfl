open Util

type result_tree =
  | Exists of (result_tree * result_tree)
  | Forall of (int * result_tree)
  | Label of string * result_tree
  | End | Fail

type result =
  | Satisfied
  | UnsatisfiedAPT of result_tree
  | Unsatisfied of (string * int) list

let rec string_of_result_tree = function
  | Exists(r1, r2) -> String.join " " ["(br_exists"; (string_of_result_tree r1); (string_of_result_tree r2); ")"]
  | Forall(0, r) -> String.join " " ["(br_forall _"; (string_of_result_tree r); ")"]
  | Forall(1, r) -> String.join " " ["(br_forall"; (string_of_result_tree r); "_ )"]
  | Forall _ -> assert false
  | Label(l, r) -> "(" ^ l ^ " " ^ string_of_result_tree r ^ ")"
  | End -> "unit"
  | Fail -> "fail"

(* from trecs-1.22/syntax.ml *)

type head = Name of string | NT of string | FD of int | CASE of int | PAIR | DPAIR
type preterm = PTapp of head * preterm list
type prerule = string * string list * preterm
type prerules = prerule list
type transition = (string * string) * string list
type transitions = transition list

let string_of_head h =
  match h with
    Name(s) -> s
  | NT(s) -> s
  | FD(n) -> (string_of_int n)
  | CASE(n) -> "_case "^(string_of_int n)
  | PAIR -> "_cons"
  | DPAIR -> "_dcons"

let rec string_of_preterm pterm =
  match pterm with
    PTapp(h, pterms) ->
      (string_of_head h)^" "^(string_of_preterms pterms)
and string_of_preterms pterms =
  match pterms with
    [] -> ""
  | pt::pterms' ->
    match pt with
      PTapp(_,[]) -> (string_of_preterm pt)^" "^(string_of_preterms pterms')
    | _ ->
      "("^(string_of_preterm pt)^") "^(string_of_preterms pterms')

let rec string_of_vars vl =
  match vl with
    [] -> ""
  | v::vl' -> v^" "^(string_of_vars vl')

let string_of_prerule (f, vl, pterm) =
  f^" "^(string_of_vars vl)^" -> "^(string_of_preterm pterm)

let rec string_of_prerules_aux prerules =
  match prerules with
    [] -> ""
  | prule::prerules' ->
    (string_of_prerule prule)^".\n"^(string_of_prerules_aux prerules')

let string_of_prerules prerules =
  "%BEGING\n"^(string_of_prerules_aux prerules)^"%ENDG\n"


let rec string_of_states qs =
  match qs with
    [] -> ""
  | q::qs' -> q^" "^(string_of_states qs')

let string_of_transition ((q,a), qs) =
  q^" "^a^" -> "^(string_of_states qs)

let rec string_of_transitions_aux trs =
  match trs with
    [] -> ""
  | tr::trs' ->
    (string_of_transition tr)^".\n"^(string_of_transitions_aux trs')

let string_of_transitions_ATA trs =
  "%BEGINATA\n"^(string_of_transitions_aux trs)^"%ENDATA\n"

let string_of_transitions trs =
  "%BEGINA\n"^(string_of_transitions_aux trs)^"%ENDA\n"
