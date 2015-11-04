open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

exception UnknownOutput

module Syntax = HorSatP_syntax

type state = string
type symbol = string
type priority = state * int

type label =
  | Ev of string
  | Br_A
  | Br_E
  | L of int
  | End

let string_of_label = function
  | Ev x -> x
  | Br_A -> "br_forall"
  | Br_E -> "br_exists"
  | L i  -> "l"^(string_of_int i)
  | End  -> "unit"

type formula =
  | Tt | Ff
  | Label of int * symbol
  | And of formula * formula
  | Or  of formula * formula

let rec string_of_formula = function
  | Tt -> "true"
  | Ff -> "false"
  | Label (i, q) -> "(" ^ string_of_int i ^ ", " ^ q ^ ")"
  | And (f1, f2) -> "(" ^ string_of_formula f1 ^ " /\\ " ^ string_of_formula f2 ^ ")"
  | Or  (f1, f2) -> "(" ^ string_of_formula f1 ^ " \\/ " ^ string_of_formula f2 ^ ")"

type transition = state * symbol * formula
type spec = transition list * priority list

type result =
  | Safe | Unsafe

let version () = "dummy"


let trans_const = function
  | Unit -> Syntax.PTapp(Syntax.Name "unit", [])
  | True -> Syntax.PTapp(Syntax.FD 0, [])
  | False -> Syntax.PTapp(Syntax.FD 1, [])
  | TreeConstr(_,s) -> Syntax.PTapp(Syntax.Name s, [])
  | c -> Format.printf "trans_const: %a@." CEGAR_print.term (Const c); assert false

let rec trans_id x =
  let map = function
    | '\'' -> "_prime_"
    | '.' -> "_dot_"
    | '&' -> "_et_"
    | c -> String.make 1 c
  in
  String.fold_left (fun s c -> s ^ map c) "" x

let rec trans_term br = function
  | Const c ->
     trans_const c
  | Var x when is_uppercase x.[0] ->
     Syntax.PTapp(Syntax.NT (trans_id x), [])
  | Var x ->
     Syntax.PTapp (Syntax.Name (trans_id x), [])
  | App(Const (Label n), t) ->
     Syntax.PTapp(Syntax.Name ("l" ^ string_of_int n), [trans_term br t])
  | App(App(App(Const If, Const RandBool), t2), t3) ->
     Syntax.PTapp(Syntax.Name br, [trans_term br t2; trans_term br t3])
  | App(App(App(Const If, t1), t2), t3) ->
     Syntax.PTapp(Syntax.CASE 2, [trans_term br t1; trans_term br t2; trans_term br t3])
  | App(t1,t2) ->
     let Syntax.PTapp(hd, ts1) = trans_term br t1 in
     let t2' = trans_term br t2 in
     Syntax.PTapp(hd, ts1@[t2'])
  | Fun _ -> assert false
  | Let _ -> assert false

let rec trans_fun_def br (f,xs,t1,es,t2) =
  let rec add_event e t =
    match e with
    | Event s -> Syntax.PTapp(Syntax.Name ("event_" ^ s), [t])
    | Branch n -> assert false(* Syntax.PTapp(Syntax.Name ("l" ^ string_of_int n), [t])*)
  in
  assert (t1 = Const True);
  trans_id f, List.map trans_id xs, List.fold_right add_event es (trans_term br t2)

let read_as_string in_channel =
  let result = ref "" in
  try
    while true do
      let line = input_line in_channel in
      result := (!result)^line
    done;
    !result
  with End_of_file ->
    close_in in_channel;
    !result


let rec verifyFile_aux filename =
  let default = "empty" in
  let result_file = Filename.change_extension !Flag.filename "horsatp_out" in
  let oc = open_out result_file in
  output_string oc default;
  close_out oc;
  let cmd = Format.sprintf "%s < %s > %s 2>/dev/null" !Flag.horsatp filename result_file in
  ignore @@ Sys.command cmd;
  let ic = open_in result_file in
  read_as_string ic

let verifyFile filename =
  let r = verifyFile_aux filename in
  Printf.eprintf "[Info] HorSatP returned \"%s\"\n" r;
  match r with
  | "Satisfied" -> Safe
  | "Unsatisfied" -> Unsafe
  | _ -> assert false

let trans_spec (delta, priority) =
  let string_of_delta ds =
    String.join "\n" (List.map (fun (q, a, formula) -> q ^ " " ^ a ^ " -> " ^ (string_of_formula formula) ^ ".") ds)
  in
  String.join "\n" [
    "%TRANSITION";
    string_of_delta delta;
    "%PRIORITY";
    (String.join ".\n" (List.map
                          (fun (q, m) -> q ^ " -> " ^ string_of_int m)
                          priority))^".";
  ]

let trans ({defs}, spec) =
  let defs':Syntax.prerules = List.map (trans_fun_def "br_forall") defs in
  let spec' = trans_spec spec in
  Syntax.string_of_prerules defs' ^ spec'

let write_log filename target =
  let cout = open_out filename in
  output_string cout target;
  close_out cout

let check target =
  let target' = trans target in
  let input = Filename.change_extension !Flag.filename "hors" in
  try
    if !Flag.debug_level > 1 then Format.printf "%s." target';
    write_log input target';
    verifyFile input
  with Failure("lex error") -> raise UnknownOutput

let gather_events defs =
  let aux (_,_,_,es,_) =
    let aux' = function
      | Event s -> "event_"^s
      | _ -> assert false in
    List.map aux' es in
  List.flatten_map aux defs

let make_apt events (a, b) =
  let q0, q1, q2 = "q0", "q1", "q2" in
  let rec trans = function
    | Ev x when x = a -> Label (1, q1)
    | Ev x when x = b -> Label (1, q2)
    | Ev x when x = "event_fail" -> Ff
    | Ev _ -> Label (1, q0)
    | Br_A -> And (Label (1, q0), Label (2, q0))
    | Br_E -> Or  (Label (1, q0), Label (2, q0))
    | L _  -> Label (1, q0)
    | End  -> Ff
  in
  let default_sym = [Br_A; Br_E; L 0; L 1; End] in
  let syms  = [Ev a; Ev b] @ events @ default_sym in
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

let make_fair_nonterm_spec labels streett : spec =
  if List.length streett <> 1 then
    (Format.eprintf "Error: size of fairness constraints list must be 1";
     assert false);
  let a, b = List.hd streett in
  let ev_a, ev_b = "event_"^a, "event_"^b in
  let events = List.filter_map
    (fun e ->
      if e <> ev_a && e <> ev_b then
        Some (Ev e)
      else
        None)
    labels in
  make_apt events (ev_a, ev_b)
