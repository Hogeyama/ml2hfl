open Util

exception TimeOut
exception LongInput

let output_csv result =
  let oc =
    open_out_gen [Open_append; Open_creat] 0o666 "mochi_exp.csv"
  in
  let ocf =
    Format.make_formatter
      (output oc)
      (fun () -> flush oc)
  in
  Format.fprintf ocf "@[<v>";
  Format.fprintf
    ocf
    "%s,\"%s\",%d,%f,%f,%f,%f@,"
    (Filename.chop_extension (Filename.basename !Flag.filename))
    (String.replace_chars
       (function '"' -> "\"\"" | c -> String.of_char c)
       result)
    !Flag.cegar_loop
    !Flag.time_abstraction
    !Flag.time_mc
    !Flag.time_cegar
    !Flag.time_parameter_inference;
  Format.fprintf ocf "@]@?";
  close_out oc

let print_info () =
  if !Flag.exp
  then
    begin
      Format.printf "{";
      Format.printf "\"filename\": %S, " !Flag.filename;
      Format.printf "\"result\": %S, " !Flag.result;
      Format.printf "\"cycles\": \"%d\", " !Flag.cegar_loop;
      (if !Flag.termination then
         begin
           Format.printf "\"ranking\": {";
           List.iter
             (fun (f_name, (cycles, pred)) ->
              Format.printf "\"%s\": {\"function\": \"%a\", \"inferCycles\": \"%d\"}, " f_name BRA_types.pr_ranking_function pred cycles)
             !Termination_loop.lrf;
           Format.printf " \"_\":{} }, "
         end
       else ());
      Format.printf "\"total\": \"%.3f\", " (get_time());
      Format.printf "\"abst\": \"%.3f\", " !Flag.time_abstraction;
      Format.printf "\"mc\": \"%.3f\", " !Flag.time_mc;
      Format.printf "\"refine\": \"%.3f\", " !Flag.time_cegar;
      Format.printf "\"exparam\": \"%.3f\"" !Flag.time_parameter_inference;
      Format.printf "}@."
    end
  else
    begin
      if !Flag.exp2 then output_csv !Flag.result;
      if !Flag.add_closure_exparam && !Flag.result = "terminating" then
        Format.printf "exparam inserted program:@. %a@." Print.term !ExtraParamInfer.origWithExparam;
      if !Flag.termination && !Flag.result = "terminating" then
        begin
          List.iter
            (fun (f_name, (cycles, pred)) ->
             Format.printf "ranking function(%s)[inference cycle: %d]: %a\n" f_name cycles BRA_types.pr_ranking_function pred;
             if !Flag.add_closure_exparam then
               let str_exparam = ExtraParamInfer.to_string_CoeffInfos pred.BRA_types.substToCoeffs in
               if str_exparam <> "" then Format.printf "exparam(%s):\n%s\n" f_name str_exparam)
            !Termination_loop.lrf
        end;
      Format.printf "cycles: %d\n" !Flag.cegar_loop;
      Format.printf "total: %.3f sec\n" @@ get_time();
      Format.printf "  abst: %.3f sec\n" !Flag.time_abstraction;
      Format.printf "  mc: %.3f sec\n" !Flag.time_mc;
      Format.printf "  refine: %.3f sec\n" !Flag.time_cegar;
      Format.printf "    exparam: %.3f sec\n" !Flag.time_parameter_inference;
      Format.pp_print_flush Format.std_formatter ()
    end


let get_commit_hash () =
  try
    let cin = open_in "COMMIT" in
    let mochi = input_line cin in
    let fpat =
      try
        Some (input_line cin)
      with End_of_file -> None
    in
    close_in cin;
    mochi, fpat
  with Sys_error _ | End_of_file -> "", None


let print_env cmd =
  let mochi,fpat = get_commit_hash () in
  let trecs_version = TrecsInterface.version () in
  Color.printf Color.Green "MoCHi: Model Checker for Higher-Order Programs@.";
  if mochi <> "" then Format.printf "  Build: %s@." mochi;
  Option.iter (Format.printf "  FPAT version: %s@.") fpat;
  if trecs_version <> "" then Format.printf "  TRecS version: %s@." @@ trecs_version;
  Format.printf "  OCaml version: %s@." Sys.ocaml_version;
  if cmd then
    begin
      Format.printf "  Command: %a@." (print_list Format.pp_print_string " ") !Flag.args;
      Format.printf "@."; ()
    end


let main in_channel =
  let input_string =
    let s = String.create Flag.max_input_size in
    let n = my_input in_channel s 0 Flag.max_input_size in
    if n = Flag.max_input_size then raise LongInput;
    String.sub s 0 n
  in

  let lb = Lexing.from_string input_string in
  lb.Lexing.lex_curr_p <-
    {Lexing.pos_fname = Filename.basename !Flag.filename;
     Lexing.pos_lnum = 1;
     Lexing.pos_cnum = 0;
     Lexing.pos_bol = 0};
  if !Flag.input_cegar then
    let open CEGAR_syntax in
    let prog = CEGAR_parser.prog CEGAR_lexer.token lb in
    let prog' = Typing.infer {prog with env=[]} in
    let env = List.filter_out (fun (f,_) -> List.mem_assoc f prog.env) prog'.env @ prog.env in
    Main_loop.run_cegar {prog with env}
  else
    let orig = Parse.use_file lb in
    Id.set_counter (Ident.current_time () + 1);
    let parsed = Parser_wrapper.from_use_file orig in
    let () =
      if true && !Flag.debug_level > 0
      then Format.printf "%a:@. @[%a@.@." Color.s_red "parsed" Print.term parsed
    in
    if !Flag.split_assert
    then
      let paths = Trans.search_fail parsed in
      let ts = List.map (fun path -> Trans.screen_fail path parsed) paths in
      List.for_all (Main_loop.run orig) (List.rev ts);
    else if !Flag.termination then
      let open BRA_util in
      (* let parsed = (BRA_transform.remove_unit_wraping parsed) in *)
      let parsed = BRA_transform.lambda_lift (BRA_transform.remove_unit_wraping parsed) in
      let _ = if !Flag.debug_level > 0 then Format.printf "lambda-lifted::@. @[%a@.@." Print.term parsed in
      let parsed = BRA_transform.regularization parsed in
      let _ = if !Flag.debug_level > 0 then Format.printf "regularized::@. @[%a@.@." Print.term parsed in
      let parsed = if !Flag.add_closure_depth then ExtraClsDepth.addExtraClsDepth parsed else parsed in
      let _ = if !Flag.debug_level > 0 && !Flag.add_closure_depth then Format.printf "closure depth inserted::@. @[%a@.@." Print.term parsed in
      let parsed = if !Flag.add_closure_exparam then ExtraParamInfer.addTemplate parsed else parsed in
      let _ = if !Flag.debug_level > 0 && !Flag.add_closure_exparam then Format.printf "closure exparam inserted::@. @[%a@.@." Print.term parsed in
      let holed_list = BRA_transform.to_holed_programs parsed in
      let result =
        try
          List.for_all
            (fun holed ->
             let init_predicate_info =
               { BRA_types.variables = List.map BRA_transform.extract_id (BRA_state.get_argvars holed.BRA_types.state holed.BRA_types.verified)
               ; BRA_types.substToCoeffs = if !Flag.add_closure_exparam then ExtraParamInfer.initPreprocessForExparam else (fun x -> x)
               ; BRA_types.prev_variables = List.map BRA_transform.extract_id (BRA_state.get_prev_statevars holed.BRA_types.state holed.BRA_types.verified)
               ; BRA_types.coefficients = []
               ; BRA_types.errorPaths = []
               ; BRA_types.errorPathsWithExparam = [] } in
             let predicate_que = Queue.create () in
             let _ = Queue.add (fun _ -> init_predicate_info) predicate_que in
             Termination_loop.reset_cycle ();
             Termination_loop.run predicate_que holed) holed_list
        with
        | Fpat.PolyConstrSolver.NoSolution
        | Termination_loop.FailedToFindLLRF -> false
      in
      if result then
        (Flag.result := "terminating"; if not !Flag.exp then Format.printf "Terminating!@."; result)
      else
        (Flag.result := "unknown"; if not !Flag.exp then Format.printf "Unknown...@."; result)
    else
      Main_loop.run orig parsed



let parse_fpat_arg arg =
  let args = Array.of_list @@ "FPAT" :: split_spaces arg in
  let usage = "Options for FPAT are:" in
  try
    Arg.parse_argv ~current:(ref 0) args (Arg.align Fpat.Config.arg_spec) ignore usage
  with
  | Arg.Bad s
  | Arg.Help s -> Format.printf "%s" s; exit 0


let print_option_and_exit = ref (fun () -> ())

let usage =
  "MoCHi: Model Checker for Higher-Order Programs\n\n" ^
    "Usage: " ^ Sys.executable_name ^ " [options] file\noptions are:"
let arg_spec =
  Arg.align
    ["-I", Arg.String (fun dir -> Config.load_path := dir::!Config.load_path),
     "<dir>  Add <dir> to the list of include directories";
     "-margin", Arg.Int Format.set_margin, "<n>  Set pretty printing margin";
     "-only-result",
     Arg.Unit (fun () ->
               Flag.only_result := true;
               Flag.debug_level := 0;
               Flag.print_progress := false),
     " Show only result";
     "-debug", Arg.Set_int Flag.debug_level, "<n>  Set debug level";
     "-debug-module",
     Arg.String (fun mods -> Flag.debug_module := String.nsplit mods "," @ !Flag.debug_module),
     "<modules>  Set debug flag of modules (comma-separated)";
     "-debug-abst", Arg.Set Flag.debug_abst, " Debugging abstraction";
     "-color", Arg.Set Flag.color, " Turn on syntax highlighting";
     "-color-always", Arg.Set Flag.color_always, " Turn on syntax highlighting even if stdout does not refer to a terminal";
     "-ignore-conf", Arg.Set Flag.ignore_conf, " Ignore option.conf";
     "-exp",
     Arg.Unit (fun () ->
               Flag.only_result := true;
               Flag.debug_level := 0;
               Flag.print_progress := false;
               Flag.exp := true),
     " For experiments";
     "-exp2",
     Arg.Unit (fun () ->
               Flag.debug_level := 0;
               Flag.exp2 := true),
     " Experiment mode (output mochi_exp.csv)";
     "-v", Arg.Unit (fun () -> print_env false; exit 0), " Print the version shortly";
     "-version", Arg.Unit (fun () -> print_env false; exit 0), " Print the version";
     "-limit", Arg.Set_int Flag.time_limit, " Set time limit";
     "-option-list", Arg.Unit (fun () -> !print_option_and_exit ()), " Print list of options (for completion)";
     "-print-non-CPS-abst", Arg.Unit (fun () -> Flag.just_print_non_CPS_abst := true; Flag.trans_to_CPS := false), " Print non-CPS abstracted program (and exit)";
     (* preprocessing *)
     "-no-exparam", Arg.Set Flag.no_exparam, " Do not add extra parameters";
     "-list-option", Arg.Set Flag.encode_list_opt, " Encode list using options not pairs";
     "-disable-preprocess", Arg.Clear Flag.init_trans, " Disable encoding of recursive data structures, CPS transformation, etc.";
     "-lift-fv", Arg.Set Flag.lift_fv_only, " Lift variables which occur in a body";
     "-cps-naive", Arg.Set Flag.cps_simpl, " Use naive CPS transformation";
     "-ins-param-funarg", Arg.Set Flag.insert_param_funarg, " Insert an extra param for functions with function arguments";
     "-tupling", Arg.Unit (fun () -> Flag.tupling := not !Flag.tupling), " Toggle tupling";
     "-elim-same-arg", Arg.Set Flag.elim_same_arg, " Eliminate same arguments";
     "-base-to-int", Arg.Set Flag.base_to_int, " Replace primitive base types with int";
     (* verifier *)
     "-it", Arg.Unit (fun _ -> Flag.cegar := Flag.CEGAR_InteractionType), " Interaction type based verifier";
     "-spec", Arg.Set_string Flag.spec_file, "<filename>  use <filename> as a specification";
     "-use-spec", Arg.Set Flag.use_spec, " use XYZ.spec for verifying XYZ.ml if exists (This option is ignored if -spec is used)";
     "-disable-comment-spec", Arg.Clear Flag.comment_spec, " disable {SPEC} on comments";
     (* CEGAR *)
     "-split-assert", Arg.Set Flag.split_assert, " Reduce to verification of multiple programs (each program has only one assertion)";
     "-dpa", Arg.Set Flag.disable_predicate_accumulation, " Disable predicate accumulation";
     (* relatively complete verification *)
     "-rc", Arg.Set Flag.relative_complete, " Enable relatively complete verification from the begining";
     "-nex", Arg.Set_int Flag.number_of_extra_params,
     " Number of inserted extra parameters for each functional argument";
     "-cc", Arg.Set Flag.enable_coeff_const,
     " Enable constant terms of extra parameters";
     (* predicate abstraction *)
     "-abs-remove-false", Arg.Set Flag.remove_false, " Do not use unsatisfiable predicates in abstraction";
     "-no-enr", Arg.Clear Flag.expand_nonrec, " Do not expand non-recursive functions";
     "-enr", Arg.Set Flag.expand_nonrec, " Expand non-recursive functions";
     "-enr2", Arg.Unit (fun _ -> Flag.expand_nonrec := true; Flag.expand_nonrec_init := false),
     " Expand non-recursive functions except those in the original program";
     "-abs-filter", Arg.Set Flag.use_filter, " Turn on the abstraction-filter option";
     "-neg-pred-off", Arg.Unit (fun _ -> Flag.never_use_neg_pred := true),
     " Never use negative predicates for abstraction";
     (* higher-order model checking *)
     "-trecs-bin", Arg.Set_string Flag.trecs,
                   Format.sprintf "<cmd>  Change trecs command to <cmd> (default: \"%s\")" !Flag.trecs;
     "-ea", Arg.Set Flag.print_eval_abst, " Print evaluation of abstacted program";
     (* predicate discovery *)
     "-fpat", Arg.String parse_fpat_arg, "<option>  Pass <option> to FPAT";
     "-bool-init-empty", Arg.Set Flag.bool_init_empty,
     " Use an empty set as the initial sets of predicates for booleans";
     "-mp", Arg.Set Flag.use_multiple_paths, " Use multiple infeasible error paths for predicate discovery";
     (* SWT solver *)
     "-cvc3-bin", Arg.Set_string Flag.cvc3,
                  Format.sprintf "<cmd>  Change cvc3 command to <cmd> (default: \"%s\")" !Flag.cvc3;
     (* termination mode *)
     "-termination-disj",
     Arg.Unit (fun _ ->
               Flag.termination := true;
               Flag.disjunctive := true),
     " Check termination by finding disjunctive well-founded relation";
     "-termination",
     Arg.Unit (fun _ ->
               Flag.termination := true),
     " Check termination";
     "-termination-sep",
     Arg.Unit (fun _ ->
               Flag.termination := true;
               Flag.separate_pred := true),
     " Check termination with separating {decrease, boundedness} verification";
     "-termination-split-callsite",
     Arg.Unit (fun _ ->
               Flag.termination := true;
               Flag.split_callsite := true),
     " Check termination for each callsite of functions";
     "-add-cd",
     Arg.Unit (fun _ ->
               Flag.add_closure_depth := true),
     " Insert extra parameters for representing depth of closures";
     "-infer-ranking-exparam",
     Arg.Unit (fun _ ->
               Flag.add_closure_exparam := true),
     " Infer extra ranking parameters for closures for termination verification"
    ]

let () = print_option_and_exit :=
           fun () ->
           List.iter (fun (s,_,_) -> Format.printf "%s " s) arg_spec; exit 0

let string_of_exception = function
  | e when Fpat.Config.is_fpat_exception e ->
     Fpat.Config.string_of_fpat_exception e
  | Syntaxerr.Error err -> "Syntaxerr.Error"
  | Typecore.Error(loc,env,err) -> "Typecore.Error"
  | Typemod.Error(loc,env,err) -> "Typemod.Error"
  | Env.Error e -> "Env.Error"
  | Typetexp.Error(loc,env,err) -> "Typetexp.Error"
  | Lexer.Error(err, loc) -> "Lexer.Error"
  | LongInput -> "LongInput"
  | TimeOut -> "TimeOut"
  | CEGAR.NoProgress -> "CEGAR.NoProgress"
  | Fatal s -> "Fatal"
  | e -> Printexc.to_string e


let parse_arg () =
  let set_file name =
    if !Flag.filename <> "" (* case of "./mochi.opt file1 file2" *)
    then (Arg.usage arg_spec usage; exit 1);
    Flag.filename := name
  in
  Arg.parse arg_spec set_file usage;
  Flag.args := Array.to_list Sys.argv;
  if not !Flag.ignore_conf
  then
    begin
      try
        let cin = open_in "option.conf" in
        let s = input_line cin in
        close_in cin;
        let args = split_spaces s in
        Arg.current := 0;
        Arg.parse_argv (Array.of_list @@ Sys.argv.(0) :: args) arg_spec set_file usage;
        Flag.args := !Flag.args @ args
      with
      | Arg.Bad s
      | Arg.Help s -> Format.printf "%s@." s; exit 1
      | Sys_error _
      | End_of_file -> ()
    end;
  if String.ends_with !Flag.filename ".cegar" then Flag.input_cegar := true;
  match !Flag.filename with
  | "" | "-" -> Flag.filename := "stdin"; stdin
  | _ -> open_in !Flag.filename


(* called before parsing options *)
let fpat_init1 () =
  Fpat.Config.set_default ()

(* called after parsing options *)
let fpat_init2 () =
  let open Fpat in
  Global.target_filename := !Flag.filename;
  Global.debug := !Flag.debug_level > 1;
  SMTProver.cvc3 := !Flag.cvc3;
  SMTProver.open_ ()

let () =
  if !Sys.interactive
  then ()
  else
    try
      Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise TimeOut));
      fpat_init1 ();
      let cin = parse_arg () in
      ignore (Unix.alarm !Flag.time_limit);
      fpat_init2 ();
      Color.init ();
      if not !Flag.only_result then print_env true;
      if main cin then decr Flag.cegar_loop;
      Fpat.SMTProver.close ();
      print_info ()
    with
    | e when !Flag.exp ->
        Format.printf "{";
        Format.printf "\"filename\": %S, " !Flag.filename;
        Format.printf "\"result\": %S, " @@ string_of_exception e;
        Format.printf "\"cycles\": \"(%d)\", " !Flag.cegar_loop;
        Format.printf "\"total\": \"(%.3f)\"" (get_time());
        Format.printf "}@."
    | e when !Flag.exp2 ->
        output_csv (string_of_exception e)
    | Fpat.AbsTypInfer.FailedToRefineTypes ->
        Format.printf "Verification failed:@.";
        Format.printf "  MoCHi could not refute an infeasible error path @.";
        Format.printf "  due to the incompleteness of the refinement type system@."
    | e when Fpat.Config.is_fpat_exception e ->
        Format.printf "FPAT: %a@." Fpat.Config.pr_exception e
    | Syntaxerr.Error err ->
        Format.printf "%a@." Syntaxerr.report_error err
    | Typecore.Error(loc,env,err) ->
        Format.printf "%a%a@." Location.print_error loc (Typecore.report_error env) err
    | Typemod.Error(loc,env,err) ->
        Format.printf "%a%a@." Location.print_error loc (Typemod.report_error env) err
    | Env.Error e -> Format.printf "%a@." Env.report_error e
    | Typetexp.Error(loc,env,err) ->
        Format.printf "%a%a@." Location.print_error loc (Typetexp.report_error env) err
    | Lexer.Error(err, loc) ->
        Format.printf "%a%a@." Location.print_error loc Lexer.report_error err
    | LongInput -> Format.printf "Input is too long@."
    | TimeOut -> Format.printf "Verification failed (time out)@."
    | CEGAR.NoProgress -> Format.printf "Verification failed (new error path not found)@."
    | CEGAR_abst.NotRefined -> Format.printf "Verification failed (new error path not found)@."
    | Fatal s ->
        Format.printf "Fatal error: %s@." s
    | Unsupported s ->
        Format.printf "Unsupported: %s@." s
