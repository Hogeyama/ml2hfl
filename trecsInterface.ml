
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

type result = Safe of Trecs.Typing.te | Unsafe of Trecs.Reduce.trace

module TS = Trecs.Syntax

let initialize () =
  Trecs.Generalize.stateid := 1;
  Trecs.Generalize.dflag := false;
  Trecs.Generalize.cache_tl2ty := [];
  Trecs.Generalize.cache_pair := [];
  Trecs.Generalize.cache_disjunct := [];
  Trecs.Generalize.cache_refinement := [];
  Trecs.Generalize.cache_tr := [];
  Trecs.Lexer.line_no := 1;
  Trecs.Lexer.end_of_previousline := 0;
  Trecs.Reduce.current_termid := 2;
  Trecs.Reduce.tinfomap := [];
  Trecs.Reduce.redcount := 0;
  Hashtbl.clear Trecs.Reduce.tidtab;
  Hashtbl.clear Trecs.Reduce.ntidtab;
  Hashtbl.clear Trecs.Reduce.appidtab;
  Hashtbl.clear Trecs.Reduce.constEtermTab;
  Hashtbl.clear Trecs.Reduce.processed;
  Trecs.Reduce.hash_typing := Hashtbl.create 10;
  Trecs.Stype.tvid := 0



let string_of_parseresult (prerules, tr) =
  (Trecs.Syntax.string_of_prerules prerules)^"\n"^(Trecs.Syntax.string_of_transitions tr)


let trans_const = function
  | Unit -> TS.PTapp(TS.Name "unit", [])
  | True -> TS.PTapp(TS.FD 0, [])
  | False -> TS.PTapp(TS.FD 1, [])
  | c -> Format.printf "print_const: %a@." CEGAR_print.print_term (Const c); assert false


let rec sanitize_id x =
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
  | Var x when is_uppercase x.[0] -> TS.PTapp(TS.NT (sanitize_id x), [])
  | Var x -> TS.PTapp (TS.Name (sanitize_id x), [])
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
  let rec aux e t =
    match e with
        Event s -> TS.PTapp(TS.Name ("event_" ^ s), [t])
      | Branch n -> TS.PTapp(TS.Name ("l" ^ string_of_int n), [t])
  in
    assert (t1 = Const True);
    sanitize_id f, xs, List.fold_right aux es (trans_term t2)

let trans_spec (q,e,qs) =
  let aux q = "q" ^ string_of_int q in
    (aux q, e), List.map aux qs

let trans ((_,defs,_),spec) =
  let defs':Trecs.Syntax.prerules = List.map trans_fun_def defs in
  let spec':Trecs.Syntax.transitions = List.map trans_spec spec in
    (defs', spec')







(*** from $(TRECS)/main.ml ***)

let parseFile filename =
  let in_strm = 
    try
      open_in filename 
    with
	Sys_error _ -> (print_string ("Cannot open file: "^filename^"\n");exit(-1)) in
  let _ = print_string ("analyzing "^filename^"...\n") in
  let _ = flush stdout in
  let lexbuf = Lexing.from_channel in_strm in
  let result =
    try
      Trecs.Parser.main Trecs.Lexer.token lexbuf
    with 
	Failure _ -> exit(-1) (*** exception raised by the lexical analyer ***)
      | Parsing.Parse_error -> (print_string "Parse error\n";exit(-1)) in
  let _ = 
    try
      close_in in_strm
    with
	Sys_error _ -> (print_string ("Cannot close file: "^filename^"\n");exit(-1)) 
  in
    result

let parseStdIn() =
  let _ = print_string ("reading standard input ...\n") in
  let in_strm = stdin in
  let lexbuf = Lexing.from_channel in_strm in
  let result =
    try
      Trecs.Parser.main Trecs.Lexer.token lexbuf
    with 
	Failure _ -> exit(-1) (*** exception raised by the lexical analyer ***)
      | Parsing.Parse_error -> (print_string "Parse error\n";exit(-1)) 
  in
    result

let factor = ref 1
let te_updated = ref false

exception LimitReached of Trecs.Typing.te

let rec verify_aux g m steps trials redexes1 freezed dmap cte nte counter =
  if trials =0 then
     raise (LimitReached (hash2list nte))
  else 
     let _ = show_time() in
     let _ = debug "reduce\n" in
     let _ = if !te_updated then (Trecs.Reduce.red_init(); te_updated := false)
             else () in
     let (redexes1',freezed) = Trecs.Reduce.red_nsteps steps redexes1 freezed g m nte cte in
     let _ = show_time() in
     let _ = debug "computing type candidates\n" in
    (*** probably we can reset tinfomap for each iteration,
     *** and just add the new type bindings to te.
     *** If te is not updated, we can continue reductions.
     ***)
     let _ = Trecs.Reduce.set_flag_queue true redexes1' in
     let _ = Trecs.Reduce.set_flag_freezed true freezed in
     let h = Trecs.Reduce.titable_create (8*steps) in
     let telist = Trecs.Reduce.tinfomap2telist !Trecs.Reduce.tinfomap h in
     let telist' = Trecs.Typing.filter_valid_types telist nte in 
     let _ = if !debugging then 
                (print_string "Candidates:\n";
                 Trecs.Typing.print_te telist')
             else () in
     let te = list2hash telist' in
     (*** for debugging 
     let _ = debug "***********\n" in
     let _ = Typing.print_te (hash2list te) in
     let _ = debug "***********\n" in
     let _ = flush(stdout) in
     ***)
     let _ = show_time() in
     let _ = debug "type checking\n" in
     let te' = Trecs.Typing.compute_te te cte nte dmap g in
     let new_telist = hash2list te' in
     let _ = (te_updated:= not(new_telist=[])) in
     let nte' = Trecs.Typing.add_te nte new_telist in
     let _ = show_time() in
     let _ = debug "type check ended\n" in
     let _ = debug "Inferred type\n" in
     let _ = if !debugging then Trecs.Typing.print_te new_telist else () in 
     let ty = Trecs.Typing.lookup_te g.Trecs.Grammar.s nte' in
       if List.mem (Trecs.Typing.ITbase(m.Trecs.Automaton.init)) ty
       then hash2list nte'
       else
(**     let te0 = init_te g.nt in
        let _ = check_eterms_in_freezed freezed te0 nte' cte in 
 **)
        (*** Reset the flags of tinfo ***)
        let _ = Trecs.Reduce.set_flag_queue false redexes1' in
        let _ = Trecs.Reduce.set_flag_freezed false freezed in
(**         verify_aux g m (min (!factor*steps) 10000) (trials-1) redexes1' freezed dmap cte nte' (counter+steps) **)
         verify_aux g m steps (trials-1) redexes1' freezed dmap cte nte' (counter+steps) 

let tcheck = ref false

let verify g m =
  let steps = !Trecs.Reduce.loop_count in
  let trials = !Trecs.Reduce.trials_limit in
  let dmap = Trecs.Grammar.mk_depend g in
  let cte = Trecs.Typing.automaton2cte m in
  let _ = if !tcheck then 
            try (Trecs.Stype.tcheck g cte; flush stdout) with
              Trecs.Stype.IllSorted s ->
                (print_string s;
                 print_string "Because the recursion scheme is not well-sorted,\n the model checking may not terminate...\n";
                 flush stdout)
  in
  let t = Trecs.Reduce.initial_redex g m.Trecs.Automaton.init in
  let visited = [] in
  let init_queue = Trecs.Reduce.enqueue_redex (t,visited) Trecs.Reduce.empty_queue in
  let nte = Trecs.Typing.init_te g.Trecs.Grammar.nt in
  let te = verify_aux g m steps trials init_queue [] dmap cte nte steps in
    (te, cte, dmap)

let verify_init g m = 
  let steps = !Trecs.Reduce.loop_count in
  let trials = !Trecs.Reduce.trials_limit in
  let dmap = Trecs.Grammar.mk_depend g in
  let cte = Trecs.Typing.automaton2cte m in
  let t = Trecs.Reduce.initial_redex g m.Trecs.Automaton.init in
  let visited = [] in
  let init_queue = Trecs.Reduce.enqueue_redex (t,visited) Trecs.Reduce.empty_queue in
    (g, m, steps, trials, init_queue, dmap, cte, steps)

let gen = ref 0

let rec report_input g m =
  let _ = print_string ("The number of rewrite rules: "^(string_of_int (List.length g.Trecs.Grammar.r))^"\n") in
  let _ = print_string ("The size of recursion scheme: "^(string_of_int (Trecs.Grammar.size_of g))^"\n") in
  let _ = print_string ("The number of states: "^(string_of_int (Trecs.Automaton.size_st m))^"\n") in
    ()

let verifyParseResult (prerules,tr) =
  let (g, m) = Trecs.Conversion.convert (prerules,tr) in
  let _ = if !debugging  then Trecs.Grammar.print_rules g.Trecs.Grammar.r else () in
  let _ = report_input g m in
    try
      let (te,cte,dmap) = verify g m in
      let _ =  (Trecs.Typing.print_te te;
                print_string ("The number of expansions: "^(string_of_int !Trecs.Reduce.redcount)^"\n"))
      in
        Safe te
    with
        Trecs.Reduce.Error tr ->
          print_string "The property is not satisfied.\nThe error trace is:\n  ";
          Trecs.Reduce.print_trace tr;
          print_string ("The number of expansions: "^(string_of_int !Trecs.Reduce.redcount)^"\n");
          Unsafe (List.rev tr)





(*
let parse_node = function
    "br" -> BrNode
  | "fail" -> FailNode
  | s when s.[0] = 'l' -> LineNode (int_of_string (String.sub s 1 (String.length s - 1)))
  | _ -> assert false
*)

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

let write_log filename target =
  let cout = open_out filename in
    output_string cout (string_of_parseresult target);
    close_out cout


let check env target =
  initialize ();
  let target' = trans target in
  let r =
    if !Flag.use_new_trecs
    then
      let input = "input.hors" in
        write_log input target';
        verifyFile input
    else
      verifyParseResult target'
  in
    match r with
        Safe te ->
          ignore (Dependent_type.trans te env);
          None
      | Unsafe tr -> Some tr



