

open Format
open Util
open CEGAR_syntax
open CEGAR_type

type node = BrNode | FailNode | LineNode of int



let parse_node = function
    "br" -> BrNode
  | "fail" -> FailNode
  | s when s.[0] = 'l' -> LineNode (int_of_string (String.sub s 1 (String.length s - 1)))
  | _ -> assert false
      

let get_pair s =
  let n1 = String.index s ',' in
  let n2 = String.index s ')' in
  let node = parse_node (String.sub s 1 (n1-1)) in
  let n = int_of_string (String.sub s (n1+1) (n2-n1-1)) in
  let s' = String.sub s (n2+1) (String.length s-n2-1) in
    node, n, s'

let rec parse_trace s =
  match s.[0] with
      '.' -> []
    | ' ' -> parse_trace (String.sub s 1 (String.length s - 1))
    | '(' ->
      let node,n,s' = get_pair s in
        (node,n) :: parse_trace s'
    | _ -> assert false

let print_const fm = function
    Fail -> Format.fprintf fm "fail"
  | Event s -> Format.fprintf fm "%s" s
  | Label n -> Format.fprintf fm "label(%d)" n
  | Unit -> Format.fprintf fm "unit"
  | True -> Format.fprintf fm "0"
  | False -> Format.fprintf fm "1"
  | If -> Format.fprintf fm "_case 2"

let print_var = Format.pp_print_string

let rec print_term fm = function
    Const c -> print_const fm c
  | Var x -> print_var fm x
  | App _ as t ->
      let t,ts = decomp_app t in
      let aux fm t =
        match t with
            App _ -> Format.fprintf fm "(%a)" print_term t
          | _ -> Format.fprintf fm "%a" print_term t
      in
        print_list aux " " false fm (t::ts)

let rec print_fun_def fm (f,xs,t1,t2) =
  assert (t1 = Const True);
  Format.fprintf fm "%a -> %a@." (print_list print_var " " false) (f::xs) print_term t2

let print_hors fm (defs, spec) =
    fprintf fm "%%BEGING\n";
    List.iter (print_fun_def fm) defs;
    fprintf fm "%%ENDG\n\n";
    fprintf fm "%%BEGINA\n";
    Automata.print_buchi fm spec;
    fprintf fm "%%ENDA\n\n"




let print_hors_to_file hors =
  let cout = open_out "log.hors" in
  let fm = formatter_of_out_channel cout in
    print_hors fm hors;
    pp_print_flush fm ();
    flush cout;
    close_out cout








let print_hors_to_file hors =
  let cout = open_out Flag.trecs_log in
  let fm = formatter_of_out_channel cout in
    print_hors fm hors;
    pp_print_flush fm ();
    flush cout;
    close_out cout





let model_check_aux ((env,defs,main),spec) =
  let cin,cout = Unix.open_process Flag.trecs in
  let fm = formatter_of_out_channel cout in
  let () = print_hors fm (defs, spec) in
  let () = pp_print_flush fm () in
  let () = close_out cout in
  let s1 = ignore (input_line cin); ignore (input_line cin); input_line cin in
  let s2 = ignore (input_line cin); input_line cin in
  let _ = close_in cin in
  let () = pp_print_flush std_formatter () in
  let () =
    match Unix.close_process (cin, cout) with
        Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> ()
  in
    if s1 = "The property is not satisfied."
    then
      let () =
        if Flag.print_trecs_output
        then printf "TRecS output: %s@.@." s2
      in
      let ce = parse_trace s2 in
      let ce' = List.flatten (List.map (function (LineNode i,_) -> [i] | _ -> []) ce) in
        Some ce'
    else
      begin
        assert (String.sub s1 (String.length s1 - 3) 3 = " : ");
        None
      end




let rec elim_non_det = function
    Const RandBool -> assert false
  | Const c -> Const c
  | Var x -> Var x
  | App(App(Const RandBool, (Const Unit|Var _)), Var f) ->
      App(App(Const Branch, App(Var f, Const True)), App(Var f, Const False))
  | App(t1,t2) -> App(elim_non_det t1, elim_non_det t2)
  
let elim_non_det (env,defs,main) =
  let aux (f,xs,t1,t2) =
    f, xs, elim_non_det t1, elim_non_det t2
  in
    env, List.map aux defs, main


let make_line_spec n q =
  let rec aux i spec =
    if i < 0
    then spec
    else aux (i-1) ((q, "l" ^ string_of_int i, [q])::spec)
  in
    aux n []

let make_spec n =
  match !Flag.mode with
      Flag.Reachability ->
        (0, "br", [0;0])::
        (0, "unit", [])::
        make_line_spec n 0
    | Flag.FileAccess -> assert false


let capitalize_var = String.capitalize

let capitalize (env,defs,main) =
  let env' = List.map (fun (f,typ) -> capitalize_var f, typ) env in
  let map = List.map (fun (f,_) -> f, Var (capitalize_var f)) env in
  let aux (f,xs,t1,t2) = capitalize_var f, xs, subst_map map t1, subst_map map t2 in
  let defs' = List.map aux defs in
  let main' = capitalize_var main in
    env', defs', main'


let rec arg_num = function
    TBase _ -> 0
  | TFun typ -> 1 + arg_num (snd (typ (Const Unit)))

let eta_expand_def env (f,xs,t1,t2) =
  let d = arg_num (List.assoc f env) - List.length xs in
  let ys = Array.to_list (Array.init d (fun _ -> new_id "x")) in
    f, xs@ys, t1, List.fold_left (fun t x -> App(t, Var x)) t2 ys

let eta_expand ((env,defs,main) : prog) : prog=
  env, List.map (eta_expand_def env) defs, main



let move_main (env,defs,main) =
  let rec aux acc = function
      [] -> List.rev acc
    | (f,xs,t1,t2)::defs when f = main -> aux (acc@[(f,xs,t1,t2)]) defs
    | def::defs -> aux (def::acc) defs
  in
    env, aux defs, main



let model_check prog n =
  let prog = CPS.trans prog in
  let prog = elim_non_det prog in
  let prog = eta_expand prog in
  let prog = capitalize prog in
  let spec = make_spec n in
    try
      model_check_aux (prog,spec)
    with
        Assert_failure(s,_,_) when s <> "" -> assert false
      | End_of_file -> (printf "\nTRecS failed@."; assert false)











