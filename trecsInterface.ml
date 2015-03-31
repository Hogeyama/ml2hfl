
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

exception UnknownOutput

type counterexample = int list

type result = Safe of (var * Inter_type.t) list | Unsafe of counterexample

type state = int
type input = string

type spec = (state * input * state list) list

module TS = Trecs_syntax

let string_of_parseresult (prerules, tr) =
  (TS.string_of_prerules prerules)^"\n"^(TS.string_of_transitions tr)


let trans_const = function
  | Unit -> TS.PTapp(TS.Name "unit", [])
  | True -> TS.PTapp(TS.FD 0, [])
  | False -> TS.PTapp(TS.FD 1, [])
  | c -> Format.printf "print_const: %a@." CEGAR_print.const c; assert false


let trans_id x =
  let map = function
    | '\'' -> "_prime_"
    | '.' -> "_dot_"
    | c -> String.make 1 c
  in
  String.replace_chars map x

let rec trans_term = function
  | Const c -> trans_const c
  | Var x when is_uppercase x.[0] -> TS.PTapp(TS.NT (trans_id x), [])
  | Var x -> TS.PTapp (TS.Name (trans_id x), [])
  | App(Const (Label n), t) -> TS.PTapp(TS.Name ("l" ^ string_of_int n), [trans_term t])
  | App(App(App(Const If, Const RandBool), t2), t3) ->
      TS.PTapp(TS.Name "br", List.map trans_term [t2; t3])
  | App(App(App(Const If, t1), t2), t3) ->
      TS.PTapp(TS.CASE 2, List.map trans_term [t1; t2; t3])
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
  (aux q, e), List.map aux qs

let trans ({defs},spec) =
  let defs':TS.prerules = List.map trans_fun_def defs in
  let spec':TS.transitions = List.map trans_spec spec in
  (defs', spec')



let trans_ce ce =
  let aux (s,_) =
    match s with
    | "unit" -> []
    | "br" -> []
    | s when s.[0] = 'l' -> [int_of_string @@ String.slice ~first:1 s]
    | s when String.starts_with s "event_" -> []
    | _ -> assert false
  in
  List.flatten_map aux ce




let get_pair s =
  let n1 = String.index s ',' in
  let n2 = String.index s ')' in
  let q = String.sub s 1 (n1-1) in
  let n = int_of_string (String.sub s (n1+1) (n2-n1-1)) in
  let s' = String.sub s (n2+1) (String.length s-n2-1) in
  (q, n), s'

let rec parse_trace s =
  match s.[0] with
  | '.' -> []
  | ' ' -> parse_trace (String.sub s 1 (String.length s - 1))
  | '(' ->
      let node,s' = get_pair s in
      node :: parse_trace s'
  | _ -> assert false

let rec verifyFile filename =
  let p1,p2 = !Flag.trecs_param1, !Flag.trecs_param2 in
  let result_file = Filename.change_extension !Flag.filename "trecs_out" in
  let oc = open_out result_file in
  let out_descr = Unix.descr_of_out_channel oc in
  let args = String.nsplit (Format.sprintf "%s -p %d %d %s" !Flag.trecs p1 p2 filename) " " in
  let pid = Unix.create_process (List.hd args) (Array.of_list args) Unix.stdin out_descr Unix.stderr in
  let _,_st =
    try
      Unix.waitpid [Unix.WUNTRACED] pid
    with e -> Unix.kill pid 14; raise e
  in
  close_out oc;
  let ic = open_in result_file in
  match Trecs_parser.output Trecs_lexer.token @@ Lexing.from_channel ic with
  | `Safe env ->
      close_in ic;
      Safe env
  | `Unsafe trace ->
      close_in ic;
      Unsafe (trans_ce trace)
  | `TimeOut ->
      if not !Flag.only_result
      then Format.printf "Restart TRecS (param: %d -> %d)@." p1 (2*p1);
      Flag.trecs_param1 := 2 * p1;
      close_in ic;
      verifyFile filename


let write_log filename target =
  let cout = open_out filename in
  output_string cout (string_of_parseresult target);
  close_out cout


let check target =
  let target' = trans target in
  let input = Filename.change_extension !Flag.filename "hors" in
  try
    write_log input target';
    verifyFile input
  with
  | Failure("lex error") -> raise UnknownOutput
  | End_of_file -> fatal "TRecS failed"


(* returen "" if the version cannot be obtained *)
let version () =
  let cin,cout = Unix.open_process (Format.sprintf "%s -help" !Flag.trecs) in
  let v =
    try
      let s = input_line cin in
      if Str.string_match (Str.regexp "TRecS \\([.0-9]+\\)") s 0
      then String.sub s (Str.group_beginning 1) (Str.group_end 1 - Str.group_beginning 1)
      else ""
    with Sys_error _ | End_of_file -> ""
  in
  ignore @@ Unix.close_process (cin, cout);
  v

let make_line_spec n q =
  let rec aux i spec =
    if i < 0
    then spec
    else aux (i-1) ((q, "l" ^ string_of_int i, [q])::spec)
  in
    aux n []

let make_file_spec () : spec =
  [0, "unit", [];
   0, "event_newr", [1];
   1, "event_read", [1];
   1, "event_close", [4];
   0, "event_neww", [2];
   2, "event_write", [2];
   2, "event_close", [4];
   2, "event_newr", [3];
   1, "event_neww", [3];
   3, "unit", [];
   3, "event_read", [3];
   3, "event_write", [3];
   3, "event_close", [3];
   4, "unit", [];]


let make_base_spec n q : spec = (q, "br", [q;q])::make_line_spec 1 q

let make_spec n : spec =
  let spec =
    match !Flag.mode with
    | Flag.Reachability
    | Flag.Termination -> (0,"unit",[])::make_base_spec n 0
    | Flag.FileAccess ->
        let spec = make_file_spec () in
        let qm = List.fold_left (fun acc (n,_,_) -> max acc n) 0 spec in
        let spec' = List.rev_flatten_map (make_base_spec n) @@ List.init (qm+1) Std.identity in
        spec @@@ spec'
    | Flag.NonTermination -> assert false
  in
  List.sort spec
