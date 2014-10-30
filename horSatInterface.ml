
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

exception UnknownOutput

type apt_transition =
  | APT_True | APT_False
  | APT_State of (int (* branch *) * int (* state ID *))
  | APT_And of apt_transition list
  | APT_Or of apt_transition list

type result = Safe of (var * Inter_type.t) list | Unsafe of (((int list) list) * (((Fpat.Idnt.t * Fpat.Pred.t list) list) list))

 module TS = Trecs_syntax

 module HS = HorSat_syntax

let string_of_arity_map arity_map =
  "%BEGINR\n" ^ String.join "\n" (List.map (fun (f, a) -> f ^ " -> " ^ string_of_int a ^ ".") arity_map) ^ "\n%ENDR\n"

let string_of_parseresult (prerules, arity_map, tr) =
  (TS.string_of_prerules prerules)^"\n"^string_of_arity_map arity_map ^ (TS.string_of_transitions tr)

let trans_const = function
  | Unit -> TS.PTapp(TS.Name "unit", [])
  | True -> TS.PTapp(TS.FD 0, [])
  | False -> TS.PTapp(TS.FD 1, [])
  | TreeConstr(_,s) -> TS.PTapp(TS.Name s, [])
  | c -> Format.printf "trans_const: %a@." CEGAR_print.term (Const c); assert false


let rec trans_id x =
  let map = function
    | '\'' -> "_prime_"
    | '.' -> "_dot_"
    | c -> String.make 1 c
  in
  String.fold_left (fun s c -> s ^ map c) "" x

let rec trans_term = function
  | Const c -> trans_const c
  | Var x when is_uppercase x.[0] -> TS.PTapp(TS.NT (trans_id x), [])
  | Var x -> TS.PTapp (TS.Name (trans_id x), [])
  | App(Const (Label n), t) -> TS.PTapp(TS.Name ("l" ^ string_of_int n), [trans_term t])
  | App(App(App(Const If, Const RandBool), t2), t3) ->
      TS.PTapp(TS.Name "br_forall", [trans_term t2; trans_term t3])
  | App(App(App(Const If, t1), t2), t3) ->
      TS.PTapp(TS.CASE 2, [trans_term t1; trans_term t2; trans_term t3])
  | App(t1,t2) ->
      let TS.PTapp(hd, ts1) = trans_term t1 in
      let t2' = trans_term t2 in
      TS.PTapp(hd, ts1@[t2'])
  | Fun _ -> assert false
  | Let _ -> assert false

let rec trans_fun_def (f,xs,t1,es,t2) =
  let rec add_event e t =
    match e with
    | Event s -> TS.PTapp(TS.Name ("event_" ^ s), [t])
    | Branch n -> assert false(* TS.PTapp(TS.Name ("l" ^ string_of_int n), [t])*)
  in
  assert (t1 = Const True);
  trans_id f, List.map trans_id xs, List.fold_right add_event es (trans_term t2)

let trans_spec (q,e,qs) =
  let aux q = "q" ^ string_of_int q in
  let parens s = "(" ^ s ^ ")" in
  let rec apt_transition_to_string is_top = function
    | APT_True -> "true"
    | APT_False -> "false"
    | APT_State(br, q) -> parens (string_of_int br ^ "," ^ aux q)
    | APT_And ts -> let s = String.join "/\\" (List.map (apt_transition_to_string false) ts) in if is_top then s else parens s
    | APT_Or ts -> let s = String.join "\\/" (List.map (apt_transition_to_string false) ts) in if is_top then s else parens s
  in
    (aux q, e), [apt_transition_to_string true qs]

let trans ({defs=defs},arity_map, spec) =
  let defs':TS.prerules = List.map trans_fun_def defs in
  let spec':TS.transitions = List.map trans_spec spec in
    (defs', arity_map, spec')







let get_pair s =
  let n1 = String.index s ',' in
  let n2 = String.index s ')' in
  let q = String.sub s 1 (n1-1) in
  let n = int_of_string (String.sub s (n1+1) (n2-n1-1)) in
  let s' = String.sub s (n2+1) (String.length s-n2-1) in
    (q, n), s'

let rec parse_trace s =
  match s.[0] with
      '.' -> []
    | ' ' -> parse_trace (String.sub s 1 (String.length s - 1))
    | '(' ->
      let node,s' = get_pair s in
        node :: parse_trace s'
    | _ -> assert false

(*
let rec verifyFile filename =
  let default = "empty" in
  let p1,p2 = !Flag.trecs_param1, !Flag.trecs_param2 in
  let result_file = "result" in
  let oc = open_out result_file in
  let () = output_string oc default in
  let () = close_out oc in
  let cmd = Format.sprintf "%s -p %d %d -o %s %s" !Flag.trecs p1 p2 result_file filename in
  let cmd' = Format.sprintf "%s | grep -q 'Verification failed (time out).'" cmd in
  let r = Sys.command cmd' in
    if r = 0
    then
      let () = Format.printf "Restart TRecS (param: %d -> %d)@." p1 (2*p1) in
      let () = Flag.trecs_param1 := 2 * p1 in
        verifyFile filename
    else
      let ic = open_in result_file in
        match input_line ic with
            "SATISFIED" ->
              close_in ic;
              Safe []
          | "VIOLATED" ->
              let s = input_line ic in
                close_in ic;
                Unsafe (parse_trace s)
          | s ->
              close_in ic;
              if r <> 0 || s = default
              then raise (Fatal "TRecS FAILED!")
              else raise (Fatal ("Unsupported TRecS output: " ^ s))
*)

(* gather error paths *)
let rec error_trace = function
    | HS.Forall(_, t) -> error_trace t
    | HS.Exists(t1, t2) -> error_trace t1 @ error_trace t2
    | HS.Label("l0", t) -> List.map (fun l -> 0 :: l) @@ error_trace t
    | HS.Label("l1", t) -> List.map (fun l -> 1 :: l) @@ error_trace t
    | HS.Label(_, t) -> error_trace t
    | HS.End | HS.Fail -> [[]]

let rec verifyFile filename =
  let default = "empty" in
  let result_file =
    try
      Filename.chop_extension !Flag.filename ^ ".trecs_out"
    with Invalid_argument "Filename.chop_extension" -> !Flag.filename ^ ".trecs_out"
  in
  let oc = open_out result_file in
  let () = output_string oc default in
  let () = close_out oc in
  let cmd = Format.sprintf "%s %s > %s" !Flag.horsat filename result_file in
  let _ = Sys.command cmd in
  let ic = open_in result_file in
  let lb = Lexing.from_channel ic in
    match HorSat_parser.output HorSat_lexer.token lb with
        `Satisfied ->
          close_in ic;
          Format.printf "Non-terminating.@.";
          exit 0
      | `Unsatisfied ce ->
          close_in ic;
          Format.printf "Unsatisfied non-terminating condition.@. Counter-example:@. %s@." (HS.string_of_result_tree ce);
          let cexs = error_trace ce in
          let ext_cexs =
            List.map
              (fun _ -> [Fpat.Idnt.V("#randint_1"), [Fpat.Pred.make [Fpat.Idnt.make "x1", Fpat.Type.mk_int;
                                                                     Fpat.Idnt.make "x2", Fpat.Type.mk_int;
                                                                     Fpat.Idnt.make "x3", Fpat.Type.mk_int]
                                                                    Fpat.Formula.mk_true]]) cexs (* TODO: Implement *)
          in
          Unsafe (cexs, ext_cexs)
(*      | `TimeOut ->
          if not !Flag.only_result
          then Format.printf "Restart TRecS (param: %d -> %d)@." p1 (2*p1);
          Flag.trecs_param1 := 2 * p1;
          verifyFile filename *)


let write_log filename target =
  let cout = open_out filename in
    output_string cout (string_of_parseresult target);
    close_out cout


let check env target =
  let target' = trans target in
  let input =
    try
      Filename.chop_extension !Flag.filename ^ ".hors"
    with Invalid_argument "Filename.chop_extension" -> !Flag.filename ^ ".hors"
  in
  try
    Format.printf "%s@." (string_of_parseresult target');
    write_log input target';
    verifyFile input
  with Failure("lex error") -> raise UnknownOutput


(* returen "" if the version cannot be obtained *)
let version () =
  let cin,cout = Unix.open_process (Format.sprintf "%s /dev/null" !Flag.horsat) in
  let v =
    try
      let s = input_line cin in
      if Str.string_match (Str.regexp "HorSat \\([.0-9]+\\)") s 0
      then String.sub s (Str.group_beginning 1) (Str.group_end 1 - Str.group_beginning 1)
      else ""
    with Sys_error _ | End_of_file -> ""
  in
  match Unix.close_process (cin, cout) with
    Unix.WEXITED(_) | Unix.WSIGNALED(_) | Unix.WSTOPPED(_) -> v
