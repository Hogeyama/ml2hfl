
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

type result = Safe of (var * Inter_type.t) list | Unsafe of (string * int) list

module TS = Trecs_syntax


let string_of_parseresult (prerules, tr) =
  (TS.string_of_prerules prerules)^"\n"^(TS.string_of_transitions tr)


let trans_const = function
  | Unit -> TS.PTapp(TS.Name "unit", [])
  | True -> TS.PTapp(TS.FD 0, [])
  | False -> TS.PTapp(TS.FD 1, [])
  | c -> Format.printf "print_const: %a@." CEGAR_print.term (Const c); assert false


let rec trans_id x =
  let map = function
      '\'' -> "_prime_"
    | '.' -> "_dot_"
    | c -> String.make 1 c
  in
  let rec trans acc s =
    if String.length s = 0
    then acc
    else trans (acc ^ map s.[0]) (String.sub s 1 (String.length s - 1))
  in
    trans "" x

let rec trans_term = function
    Const c -> trans_const c
  | Var x when is_uppercase x.[0] -> TS.PTapp(TS.NT (trans_id x), [])
  | Var x -> TS.PTapp (TS.Name (trans_id x), [])
  | App(Const (Label n), t) -> TS.PTapp(TS.Name ("l" ^ string_of_int n), [trans_term t])
  | App(App(App(Const If, Const RandBool), t2), t3) ->
      TS.PTapp(TS.Name "br", [trans_term t2; trans_term t3])
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
        Event s -> TS.PTapp(TS.Name ("event_" ^ s), [t])
      | Branch n -> assert false(* TS.PTapp(TS.Name ("l" ^ string_of_int n), [t])*)
  in
    assert (t1 = Const True);
    trans_id f, List.map trans_id xs, List.fold_right add_event es (trans_term t2)

let trans_spec (q,e,qs) =
  let aux q = "q" ^ string_of_int q in
    (aux q, e), List.map aux qs

let trans ({defs=defs},spec) =
  let defs':TS.prerules = List.map trans_fun_def defs in
  let spec':TS.transitions = List.map trans_spec spec in
    (defs', spec')







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

let rec verifyFile filename =
  let default = "empty" in
  let p1,p2 = !Flag.trecs_param1, !Flag.trecs_param2 in
  let result_file = Filename.chop_extension !Flag.filename ^ ".trecs_out" in
  let oc = open_out result_file in
  let () = output_string oc default in
  let () = close_out oc in
  let cmd = Format.sprintf "%s -p %d %d %s > %s" !Flag.trecs p1 p2 filename result_file in
  let cmd' = Format.sprintf "%s | grep -q 'Verification failed (time out).'" cmd in
  let _ = Sys.command cmd' in
  let ic = open_in result_file in
  let lb = Lexing.from_channel ic in
    match Trecs_parser.output Trecs_lexer.token lb with
        `Safe env ->
          close_in ic;
          Safe env
      | `Unsafe trace ->
          close_in ic;
          Unsafe trace
      | `TimeOut ->
          if not !Flag.only_result
          then Format.printf "Restart TRecS (param: %d -> %d)@." p1 (2*p1);
          Flag.trecs_param1 := 2 * p1;
          verifyFile filename


let write_log filename target =
  let cout = open_out filename in
    output_string cout (string_of_parseresult target);
    close_out cout


let check env target =
  let target' = trans target in
  let input = Filename.chop_extension !Flag.filename ^ ".hors" in
    write_log input target';
    verifyFile input
