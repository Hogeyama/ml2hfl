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

type spec = (string * int) list * (int * string * apt_transition) list

type counterexample = int list list * ((int * bool list) list) list

type result = Safe of (var * Inter_type.t) list | Unsafe of counterexample

module HS = HorSat_syntax

let string_of_arity_map arity_map =
  "%BEGINR\n" ^ String.join "\n" (List.map (fun (f, a) -> f ^ " -> " ^ string_of_int a ^ ".") arity_map) ^ "\n%ENDR\n"

let string_of_parseresult (prerules, arity_map, tr) =
  (HS.string_of_prerules prerules)^"\n"^string_of_arity_map arity_map ^ (HS.string_of_transitions tr)

let trans_const = function
  | Unit -> HS.PTapp(HS.Name "unit", [])
  | True -> HS.PTapp(HS.FD 0, [])
  | False -> HS.PTapp(HS.FD 1, [])
  | TreeConstr(_,s) -> HS.PTapp(HS.Name s, [])
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
  | Var x when is_uppercase x.[0] -> HS.PTapp(HS.NT (trans_id x), [])
  | Var x -> HS.PTapp (HS.Name (trans_id x), [])
  | App(Const (Label n), t) -> HS.PTapp(HS.Name ("l" ^ string_of_int n), [trans_term t])
  | App(App(App(Const If, Const RandBool), t2), t3) ->
      HS.PTapp(HS.Name "br_forall", [trans_term t2; trans_term t3])
  | App(App(App(Const If, t1), t2), t3) ->
      HS.PTapp(HS.CASE 2, [trans_term t1; trans_term t2; trans_term t3])
  | App(t1,t2) ->
      let HS.PTapp(hd, ts1) = trans_term t1 in
      let t2' = trans_term t2 in
      HS.PTapp(hd, ts1@[t2'])
  | Fun _ -> assert false
  | Let _ -> assert false

let rec trans_fun_def (f,xs,t1,es,t2) =
  let rec add_event e t =
    match e with
    | Event s -> HS.PTapp(HS.Name ("event_" ^ s), [t])
    | Branch n -> assert false(* HS.PTapp(HS.Name ("l" ^ string_of_int n), [t])*)
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

let trans ({defs}, (arity_map, spec)) =
  let defs':HS.prerules = List.map trans_fun_def defs in
  let spec':HS.transitions = List.map trans_spec spec in
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

type path = int list (* branch info. *) * bool list (* partially constructed external input info. *) * ((int * (bool list)) list) (* external input info. *)
let add_next_rand_info r (branch,bs,ext) = (branch, [], (decomp_randint_label r, bs) :: ext)
let add_tf_info b (branch,bs,ext) = (branch,b::bs,ext)
let add_branch_info b (branch,bs,ext) = (b::branch,bs,ext)

(* gather error paths *)
let rec error_trace_aux = function
    | HS.Forall(_, t) -> error_trace_aux t
    | HS.Exists(t1, t2) -> error_trace_aux t1 @ error_trace_aux t2
    | HS.Label("l0", t) -> List.map (add_branch_info 0) @@ error_trace_aux t
    | HS.Label("l1", t) -> List.map (add_branch_info 1) @@ error_trace_aux t
    | HS.Label("true", t) -> List.map (add_tf_info true) @@ error_trace_aux t
    | HS.Label("false", t) -> List.map (add_tf_info false) @@ error_trace_aux t
    | HS.Label(r, t) when is_randint_label r -> List.map (add_next_rand_info r) @@ error_trace_aux t
    | HS.Label(_, t) -> error_trace_aux t
    | HS.End | HS.Fail -> [([],[],[])]

let error_trace tr =
  List.fold_left (fun (xs,ys) (x,_,y) -> (x::xs, y::ys)) ([],[]) @@ error_trace_aux tr

let rec verifyFile filename =
  let debug = !Flag.debug_level > 0 in
  let default = "empty" in
  let result_file = Filename.change_extension !Flag.filename "trecs_out" in
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
          Safe []
      | `Unsatisfied ce ->
          close_in ic;
          if debug then Format.printf "Unsatisfied non-terminating condition.@. Counter-example:@. %s@." (HS.string_of_result_tree ce);
          let cexs, ext_cexs = error_trace ce in
	  let ppppp fm (n, l) = Format.fprintf fm "[%d: %a]" n (print_list Format.pp_print_bool ",") l in
	  if debug then List.iter2 (fun c1 c2 -> Format.printf "FOUND:  %a | %a@." (print_list (fun fm n -> Format.fprintf fm (if n=0 then "then" else "else")) ",") c1 (print_list ppppp ",") c2) cexs ext_cexs;
          (*let ext_cexs = List.map (fun _ -> [Fpat.Idnt.V("tmp"), []]) cexs (* TODO: Implement *) in*)
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


let check target =
  let target' = trans target in
  let input = Filename.change_extension !Flag.filename "hors" in
  try
    if !Flag.debug_level > 1 then Format.printf "%s@." (string_of_parseresult target');
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

let rec make_label_spec = function
  | [] -> []
  | r::rs -> (0, r, APT_State(1, 0)) :: make_label_spec rs

let make_apt_spec labels =
  let spec =
    (0,"event_fail", APT_False)
    ::(0,"unit", APT_True)
    ::(0, "l0", APT_State(1, 0))
    ::(0, "l1", APT_State(1, 0))
    ::(0, "true", APT_State(1, 0))
    ::(0, "false", APT_State(1, 0))
    ::(0,"br_forall", APT_And([APT_State(1, 0); APT_State(2, 0)]))
    ::(0,"br_exists", APT_Or([APT_State(1, 0); APT_State(2, 0)]))::make_label_spec labels
  in
  List.sort spec

let make_arity_map labels =
  let init = [("br_forall", 2); ("br_exists", 2); ("event_fail", 1); ("unit", 0); ("true", 1); ("false", 1); ("l0", 1); ("l1", 1)] in
  let funs_map = List.map (fun l -> (l, 1)) labels in
  init @ funs_map

let make_spec labels =
  make_arity_map labels, make_apt_spec labels
