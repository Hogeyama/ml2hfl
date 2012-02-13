open Utilities;;
open Grammar;;
open Automaton;;
open Typing;;
open Reduce;;

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
      Parser.main Lexer.token lexbuf
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
      Parser.main Lexer.token lexbuf
    with 
	Failure _ -> exit(-1) (*** exception raised by the lexical analyer ***)
      | Parsing.Parse_error -> (print_string "Parse error\n";exit(-1)) 
  in
    result

let factor = ref 1
let te_updated = ref false

exception LimitReached of Typing.te

let rec verify_aux g m steps trials redexes1 freezed dmap cte nte counter =
  if trials =0 then
     raise (LimitReached (hash2list nte))
  else 
     let _ = show_time() in
     let _ = debug "reduce\n" in
     let _ = if !te_updated then (Reduce.red_init(); te_updated := false)
             else () in
     let (redexes1',freezed) = Reduce.red_nsteps steps redexes1 freezed g m nte cte in
     let _ = show_time() in
     let _ = debug "computing type candidates\n" in
    (*** probably we can reset tinfomap for each iteration,
     *** and just add the new type bindings to te.
     *** If te is not updated, we can continue reductions.
     ***)
     let _ = Reduce.set_flag_queue true redexes1' in
     let _ = Reduce.set_flag_freezed true freezed in
     let h = titable_create (8*steps) in
     let telist = tinfomap2telist !tinfomap h in
     let telist' = Typing.filter_valid_types telist nte in 
     let _ = if !debugging then 
                (print_string "Candidates:\n";
                 print_te telist')
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
     let te' = compute_te te cte nte dmap g in
     let new_telist = hash2list te' in
     let _ = (te_updated:= not(new_telist=[])) in
     let nte' = add_te nte new_telist in
     let _ = show_time() in
     let _ = debug "type check ended\n" in
     let _ = debug "Inferred type\n" in
     let _ = if !debugging then print_te new_telist else () in 
     let ty = lookup_te g.s nte' in
       if List.mem (ITbase(m.init)) ty
       then hash2list nte'
       else
(**     let te0 = init_te g.nt in
        let _ = check_eterms_in_freezed freezed te0 nte' cte in 
 **)
        (*** Reset the flags of tinfo ***)
        let _ = Reduce.set_flag_queue false redexes1' in
        let _ = Reduce.set_flag_freezed false freezed in
(**         verify_aux g m (min (!factor*steps) 10000) (trials-1) redexes1' freezed dmap cte nte' (counter+steps) **)
         verify_aux g m steps (trials-1) redexes1' freezed dmap cte nte' (counter+steps) 

let tcheck = ref false

let verify g m =
  let steps = !Reduce.loop_count in
  let trials = !Reduce.trials_limit in
  let dmap = mk_depend g in
  let cte = automaton2cte m in
  let _ = if !tcheck then 
            try (Stype.tcheck g cte; flush stdout) with
              Stype.IllSorted s ->
                (print_string s;
                 print_string "Because the recursion scheme is not well-sorted,\n the model checking may not terminate...\n";
                 flush stdout)
  in
  let t = initial_redex g m.init in
  let visited = [] in
  let init_queue = Reduce.enqueue_redex (t,visited) empty_queue in
  let nte = init_te g.nt in
  let te = verify_aux g m steps trials init_queue [] dmap cte nte steps in
    (te, cte, dmap)

let verify_init g m = 
  let steps = !Reduce.loop_count in
  let trials = !Reduce.trials_limit in
  let dmap = mk_depend g in
  let cte = automaton2cte m in
  let t = initial_redex g m.init in
  let visited = [] in
  let init_queue = Reduce.enqueue_redex (t,visited) empty_queue in
    (g, m, steps, trials, init_queue, dmap, cte, steps)

let gen = ref 0

let rec report_input g m =
  let _ = print_string ("The number of rewrite rules: "^(string_of_int (List.length g.r))^"\n") in
  let _ = print_string ("The size of recursion scheme: "^(string_of_int (Grammar.size_of g))^"\n") in
  let _ = print_string ("The number of states: "^(string_of_int (Automaton.size_st m))^"\n") in
    ()

let verifyParseResult (prerules,tr) = 
  let (g, m) = Conversion.convert (prerules,tr) in
  let _ = if !debugging  then print_rules g.r else () in
  let _ = report_input g m in
  try
    let (te,cte,dmap) = verify g m in
    let _ =  (print_te te;
       print_string ("The number of expansions: "^(string_of_int !Reduce.redcount)^"\n"))
    in
      if !gen>0 then
        (print_string ("Generalizing ...\n");
         flush stdout;
        Generalize.generalize te cte dmap g !gen []
         )
      else
        ()
  with
    (Reduce.Error tr) -> 
         (print_string "The property is not satisfied.\nThe error trace is:\n  ";
          Reduce.print_trace tr;
          print_string ("The number of expansions: "^(string_of_int !Reduce.redcount)^"\n"))
  | Reduce.GiveUp -> (print_string "Verification failed (too many candidate types; try the non-sharing mode (-ns)).\n")
  | LimitReached te -> 
     (if !debugging then
        (print_string "Inferred types so far:\n";
         print_te te)
      else ();
      print_string "Verification failed (time out).\n")
  | Grammar.UndefinedNonterminal f -> (print_string ("Undefined non-terminal: "^f^"\n"))
  | Reduce.TArityMismatch (a) ->
       print_string ("Arity mismatch on a terminal symbol: "^a^"\n")
  | Reduce.ArityMismatch (f, farg,arg) ->
       print_string ("Arity mismatch: the arity of function "^f^
                     " is "^(string_of_int(farg))^
                 " but the number of the actual arguments is "^(string_of_int(arg))^"\n")

let string_of_parseresult (prerules, tr) =
  (Syntax.string_of_prerules prerules)^"\n"^(Syntax.string_of_transitions tr)

let suicide() =
  let pid = Unix.getpid() in
    Unix.kill pid Sys.sigabrt 

exception LogFile

let web = ref false 

let rec create_file n =
  if n=0 then
     (print_string "Cannot open a log file\n"; raise LogFile)
  else
     try
      let n = Random.int(15) in
      let prefix = if !web then "/home/koba/horsmc/log/log" else "log" in
      let filename = prefix^(string_of_int n)^".hrs" in
      let fp = open_out_gen [Open_wronly;Open_creat;Open_excl;Open_trunc] 0o666 filename in
         (filename, fp)
     with
       _ -> create_file (n-1)
        
let write_log parseresult =
  let s = string_of_parseresult parseresult in
  let _ = Random.init(int_of_float(Unix.time())) in
  let (filename, fp) = create_file 3 in
  let _ = output_string fp s in
    (close_out fp; filename)

let rec loop() = loop()

let report_usage () =
    (print_string "Usage: \n";
     print_string "trecs <option>* <filename> \n\n";
     print_string "-p <num1> <num2>\n";
     print_string "  <num1>: the maximum number of expansions at each iteration\n";
     print_string "  <num2>: the number of cycles\n";
     print_string "-st \n";
     print_string "  enable sort check\n"
    )

let rec read_options index =
  match Sys.argv.(index) with
    "-st" -> (tcheck := true; read_options (index+1))
  | "-nr" -> (Reduce.recmode := false; read_options (index+1))
  | "-ns" -> (Reduce.sharing := false; read_options (index+1))
  | "-g" -> (gen := int_of_string(Sys.argv.(index+1));
             read_options (index+2))
  | "-help" -> (report_usage(); exit(-1))
  | "-w" -> (web := true; read_options (index+1))
  | "-d" -> (Utilities.debugging := true; read_options (index+1))
  | "-b" -> (Reduce.depthfirst := false; factor := 2; read_options (index+1))
  | "-bnp" -> (Reduce.depthfirst := false; factor := 2; Reduce.pruning := false; read_options(index+1))
  | "-m" -> (Reduce.mixed := true; read_options(index+1))
  | "-d2" -> (Reduce.depth2 := true; read_options(index+1))
  | "-b2" -> (Reduce.breadth2 := true; read_options(index+1))
  | "-p" -> (Reduce.loop_count := int_of_string(Sys.argv.(index+1));
             Reduce.trials_limit := int_of_string(Sys.argv.(index+2));
             read_options(index+3))
  | _ -> index


let main () =
  let _ = print_string "TRecS 1.22: Type-based model checker for higher-order recursion schemes\n" in
  let start_t = Sys.time() in
  let (index,flag) = 
      try
        (read_options 1, true)
      with
        Invalid_argument _ -> (0,false)
      | _ -> 
         (print_string "Invalid options\n\n";
          report_usage();
          exit (-1))
  in
  let parseresult =
    try
      if flag then
         parseFile(Sys.argv.(index))
      else
         parseStdIn()
    with
	Lexer.LexError s -> (print_string ("lex error: "^s^"\n"); exit (-1))
  in
 let _ = if !web then Unix.alarm 3 else 0 in
  let logfile = write_log parseresult in
    ((* Sys.set_signal Sys.sigalrm (Sys.Signal_handle(fun sigid -> write_log (string_of_parseresult parseresult))); *)
      (* loop();*)  (** for testing logging **)
     verifyParseResult parseresult;
     let end_t = Sys.time() in
       (print_string ("Elapsed Time: "^(string_of_float (end_t -. start_t))^"sec\n");
        flush stdout;
        Unix.unlink logfile)
    );;

if !Sys.interactive then
  ()
else
  main();;




