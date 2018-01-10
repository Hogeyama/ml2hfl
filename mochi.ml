open Util

let print_info_default () =
  if !Flag.Termination.add_closure_exparam && !Flag.Log.result = "terminating" then
    Format.printf "exparam inserted program:@. %a@." Print.term !ExtraParamInfer.origWithExparam;
  if Flag.Method.(!mode = Termination) && !Flag.Log.result = "terminating" then
    begin
      List.iter
        (fun (f_name, (cycles, pred)) ->
         Format.printf "ranking function(%s)[inference cycle: %d]: %a\n" f_name cycles BRA_types.pr_ranking_function pred;
         if !Flag.Termination.add_closure_exparam then
           let str_exparam = ExtraParamInfer.to_string_CoeffInfos pred.BRA_types.substToCoeffs in
           if str_exparam <> "" then Format.printf "exparam(%s):\n%s\n" f_name str_exparam)
        !Termination_loop.lrf
    end;
  if Flag.Method.(!mode = FairTermination)
  then Format.printf "cycles: %d@." !Flag.FairTermination.loop_count;
  Format.printf "CEGAR-cycles: %d@." !Flag.Log.cegar_loop;
  Format.printf "total: %.3f sec@." !!Time.get;
  Format.printf "  abst: %.3f sec@." !Flag.Log.time_abstraction;
  Format.printf "  mc: %.3f sec@." !Flag.Log.time_mc;
  Format.printf "  refine: %.3f sec@." !Flag.Log.time_cegar;
  if !Flag.Method.relative_complete then
    Format.printf "    exparam: %.3f sec@." !Flag.Log.time_parameter_inference;
  Format.pp_print_flush Format.std_formatter ()

let output_csv filename =
  let oc = open_out_gen [Open_append; Open_creat] 0o644 filename in
  let ocf = Format.formatter_of_out_channel oc in
  let pr fmt = Format.fprintf ocf fmt in
  let pr_mod fmt = if !Flag.Method.modular then Format.fprintf ocf fmt else Format.ifprintf ocf fmt in
  pr "%s," @@ Filename.chop_extension_if_any @@ Filename.basename !!Flag.mainfile;
  pr "%S," !Flag.Log.result;
  pr "%d," !Flag.Log.cegar_loop;
  pr "%f," !!Time.get;
  pr "%f," !Flag.Log.time_abstraction;
  pr "%f," !Flag.Log.time_mc;
  pr "%f," !Flag.Log.time_cegar;
  pr "%f," !Flag.Log.time_parameter_inference;
  pr_mod "%n," !Modular.num_tycheck;
  pr_mod "%f," !Modular.time_check;
  pr_mod "%f," !Modular.time_synthesize;
  pr "0@.";
  close_out oc

let output_json filename =
  let oc =
    if filename = "-" then
      stdout
    else
      open_out_gen [Open_append; Open_creat] 0o644 filename
  in
  let ocf = Format.formatter_of_out_channel oc in
  let pr fmt = Format.fprintf ocf fmt in
  let pr_ter fmt = if Flag.Method.(!mode = Termination) then Format.fprintf ocf fmt else Format.ifprintf ocf fmt in
  let pr_mod fmt = if !Flag.Method.modular then Format.fprintf ocf fmt else Format.ifprintf ocf fmt in
  pr "{filename: %S" !!Flag.mainfile;
  pr ", result: %S" !Flag.Log.result;
  pr ", cycles: %d" !Flag.Log.cegar_loop;
  pr_ter ", ranking: {";
  List.iter
    (fun (f_name, (cycles, pred)) ->
     pr_ter ", %S: {function: \"%a\", inferCycles: %d}" f_name BRA_types.pr_ranking_function pred cycles)
    !Termination_loop.lrf;
  pr_ter "\"_\":{} }";
  pr ", total: %f" !!Time.get;
  pr ", abst: %f" !Flag.Log.time_abstraction;
  pr ", mc: %f" !Flag.Log.time_mc;
  pr ", refine: %f" !Flag.Log.time_cegar;
  if !Flag.Method.relative_complete then
    pr ", exparam: %f" !Flag.Log.time_parameter_inference;
  pr_mod ", \"#typeChecker\": %d" !Modular.num_tycheck;
  pr_mod ", typeChecker: %f" !Modular.time_check;
  pr_mod ", typeSynthesizer: %f" !Modular.time_synthesize;
  pr "}@."

let print_info_modular () =
  Format.printf "#typeChecker: %d@." !Modular.num_tycheck;
  Format.printf "total: %.3f sec@." !!Time.get;
  Format.printf "  typeChecker: %.3f sec@." !Modular.time_check;
  Format.printf "  typeSynthesizer: %.3f sec@." !Modular.time_synthesize

let output_exp () =
  Option.iter output_csv !Flag.Log.output_csv;
  Option.iter output_json !Flag.Log.output_json

let print_info () =
  output_exp ();
  if !Flag.Print.result then
    if !Flag.Method.modular then
      print_info_modular ()
    else
      print_info_default ()


let print_env cmd json =
  let mochi = Revision.mochi in
  let fpat = Revision.fpat in
  let z3 =
    let a,b,c,d = Z3native.get_version () in
    Format.sprintf "%d.%d.%d.%d" a b c d
  in
  let trecs = TrecsInterface.version () in
  let horsat = HorSatInterface.version () in
  let horsat2 = HorSat2Interface.version () in
  let horsatp = HorSatPInterface.version () in
  if json then
    try
      let mochi = Option.get mochi in
      Format.printf "{Build:%S," @@ String.sub mochi 0 (String.index mochi ' ');
      Format.printf "FPAT:%S," @@ Option.get fpat;
      Format.printf "Z3:%S," z3;
      Format.printf "TRecS:%S," @@ Option.get trecs;
      Format.printf "HorSat:%S," @@ Option.get horsat;
      Format.printf "HorSat2:%S," @@ Option.get horsat2;
      Format.printf "HorSatP:%S," @@ Option.get horsatp;
      Format.printf "OCaml:%S}" Sys.ocaml_version;
    with Option.No_value -> exit 1
  else
    begin
      Color.printf Color.Green "MoCHi: Model Checker for Higher-Order Programs@.";
      Option.iter (Format.printf "  Build: %s@.") mochi;
      Option.iter (Format.printf "  FPAT revision: %s@.") fpat;
      Format.printf "  Z3 version: %s@." z3;
      Option.iter (Format.printf "  TRecS version: %s@.") trecs;
      Option.iter (Format.printf "  HorSat version: %s@.") horsat;
      Option.iter (Format.printf "  HorSat2 version: %s@.") horsat2;
      Option.iter (Format.printf "  HorSatP version: %s@.") horsatp;
      Format.printf "  OCaml version: %s@." Sys.ocaml_version;
      if cmd then
        !Flag.Log.args
        |> List.map (fun s -> if String.contains s ' ' then Format.sprintf "'%s'" s else s)
        |> Format.printf "  Command: %a@.@." (print_list Format.pp_print_string " ")
    end



let main_input_cegar lb =
  let open CEGAR_syntax in
  let prog = CEGAR_parser.prog CEGAR_lexer.token lb in
  let prog' = Typing.infer {prog with env=[]} in
  let env = List.filter_out (fun (f,_) -> List.mem_assoc f prog.env) prog'.env @ prog.env in
  Main_loop.run_cegar {prog with env}

let main_split_assert orig spec parsed =
  let paths = Trans.search_fail parsed in
  let ts = List.map (Trans.screen_fail -$- parsed) paths in
  List.for_all (Main_loop.run orig ~spec) (List.rev ts)

let main_termination orig parsed =
  let open BRA_util in
  (* let parsed = (BRA_transform.remove_unit_wraping parsed) in *)
  let parsed = BRA_transform.lambda_lift (BRA_transform.remove_unit_wraping parsed) in
  let _ = Verbose.printf "lambda-lifted::@. @[%a@.@." Print.term parsed in
  let parsed = BRA_transform.regularization parsed in
  let _ = Verbose.printf "regularized::@. @[%a@.@." Print.term parsed in
  let parsed = if !Flag.Termination.add_closure_depth then ExtraClsDepth.addExtraClsDepth parsed else parsed in
  let _ = if !Flag.Termination.add_closure_depth then Verbose.printf "closure depth inserted::@. @[%a@.@." Print.term parsed in
  let parsed = if !Flag.Termination.add_closure_exparam then ExtraParamInfer.addTemplate parsed else parsed in
  let _ = if !Flag.Termination.add_closure_exparam then Verbose.printf "closure exparam inserted::@. @[%a@.@." Print.term parsed in
  let holed_list = BRA_transform.to_holed_programs parsed in
  let result =
    try
      List.for_all
        (fun holed ->
         let init_predicate_info =
           { BRA_types.variables = List.map BRA_transform.extract_id (BRA_state.get_argvars holed.BRA_types.state holed.BRA_types.verified)
           ; BRA_types.substToCoeffs = if !Flag.Termination.add_closure_exparam then ExtraParamInfer.initPreprocessForExparam else (fun x -> x)
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
    (Flag.Log.result := "terminating"; Format.printf "Terminating!@."; result)
  else
    (Flag.Log.result := "unknown"; Format.printf "Unknown...@."; result)

let main_fair_termination orig spec parsed =
  let result = Fair_termination.run spec parsed in
  if result
  then Format.printf "Fair terminating!@.@."
  else Format.printf "Unknown...@.@.";
  result

let main_module_verification orig spec parsed =
  assert false

let output_randint_refinement_log input_string =
  let cout =
    let input =
      let dirname = Filename.dirname !!Flag.mainfile in
      let basename = Filename.basename !!Flag.mainfile in
      dirname ^ "/refinement/" ^ Filename.change_extension basename "refinement"
    in
    open_out_gen [Open_wronly; Open_trunc; Open_text; Open_creat] 0o644 input
  in
  output_string cout ("[INPUT]:\n" ^ input_string ^ "\n");
  close_out cout


let main cin =
  let input_string =
    let s = IO.input_all cin in
    if Flag.Method.(!mode = FairTermination || !mode = FairNonTermination)
    then Fair_termination_util.add_event s
    else s
  in
  let lb = Lexing.from_string input_string in
  lb.Lexing.lex_curr_p <-
    {Lexing.pos_fname = Filename.basename !!Flag.mainfile;
     Lexing.pos_lnum = 1;
     Lexing.pos_cnum = 0;
     Lexing.pos_bol = 0};
  if !Flag.Method.input_cegar then
    main_input_cegar lb
  else
    let orig = Parse.use_file lb in
    Id.set_counter (Ident.current_time () + 1000);
    let parsed = Parser_wrapper.from_use_file orig in
    Verbose.printf "%a:@. @[%a@.@." Color.s_red "parsed" Print.term_typ parsed;
    if !Flag.NonTermination.randint_refinement_log
    then output_randint_refinement_log input_string;
    let spec = Spec.read Spec_parser.spec Spec_lexer.token |@> Verbose.printf "%a@." Spec.print in
    let check_safety t =
      if !Flag.Method.modular then
        Modular.main orig spec t
      else
        Main_loop.run orig ~spec t
    in
    if !Flag.Method.split_assert then
      main_split_assert orig spec parsed
    else if !Flag.Method.verify_ref_typ then
      Verify_ref_typ.main orig spec parsed
    else if Flag.Method.(!mode = Termination) then
      main_termination orig parsed
    else if Flag.Method.(!mode = FairTermination) then
      main_fair_termination orig spec parsed
    else if !Flag.Mode.module_mode then
      Verify_module.main check_safety parsed
    else
      check_safety parsed


let set_exp_filename filename =
  if Filename.check_suffix filename ".csv" then
    Flag.Log.output_csv := Some filename
  else if Filename.check_suffix filename ".json" then
    Flag.Log.output_json := Some filename
  else if filename = "-" then
    begin
      set_only_result ();
      Flag.Print.result := false;
      Flag.Log.output_json := Some filename
    end
  else
    unsupported "Experimental results file type"

let usage =
  "MoCHi: Model Checker for Higher-Order Programs\n\n" ^
    "Usage: " ^ Sys.executable_name ^ " [options] file\noptions are:"
let rec arg_spec () =
  Arg.align
    ["-I", Arg.String (fun dir -> Config.load_path := dir::!Config.load_path),
     "<dir>  Add <dir> to the list of include directories";
     "-margin", Arg.Int Format.set_margin, "<n>  Set pretty printing margin";
     "-only-result", Arg.Unit set_only_result, " Show only result";
     "-debug", Arg.String set_debug_modules, "<modules>  Set debug flag of modules (comma-separated)";
     "-debug-abst", Arg.Set Flag.Debug.abst, " Debugging abstraction";
     "-color", Arg.Set Flag.PrettyPrinter.color, " Turn on syntax highlighting";
     "-color-always", Arg.Set Flag.PrettyPrinter.color_always, " Turn on syntax highlighting even if stdout does not refer to a terminal";
     "-ignore-conf", Arg.Set Flag.Mode.ignore_conf, " Ignore option.conf";
     "-exp", Arg.String set_exp_filename, "<filename>  Output experimental results to <filename> (Support file types: *.csv, *.json)";
     "-exp-json", Arg.Unit (fun () -> set_exp_filename "-"), " Output experimental results to starndard output as JSON";
     "-v", Arg.Unit (fun () -> print_env false false; exit 0), " Print the version shortly";
     "-env", Arg.Unit (fun () -> print_env false true; exit 0), " Print the version and the environment as JSON";
     "-version", Arg.Unit (fun () -> print_env false false; exit 0), " Print the version";
     "-limit", Arg.Set_int Flag.time_limit, " Set time limit";
     "-pp", Arg.String (fun pp -> Flag.pp := Some pp), " Set preprocessor command";
     "-web", Arg.Set Flag.PrettyPrinter.web, " Web mode";
     (* abstraction *)
     "", Arg.Unit ignore, "Options_for_abstraction";
     "-ignore-exn-arg", Arg.Set Flag.Method.ignore_exn_arg, " Ignore exception arguments";
     "-abst-list-literal", Arg.Set_int Flag.Method.abst_list_literal, " Abstract long list literals";
     "-ignore-non-termination", Arg.Set Flag.Method.ignore_non_termination, " Ignore non-termination";
     (* completion *)
     "", Arg.Unit ignore, "Options_for_completion";
     "-option-list", Arg.Unit print_option_and_exit, " Print list of options";
     "-module-list", Arg.Unit (fun _ -> Format.printf "%a" (print_list Format.pp_print_string " ") !Flag.Debug.debuggable_modules; exit 0), " Print list of debuggable modules";
     (* printing *)
     "", Arg.Unit ignore, "Options_for_printing";
     "-print-abst-types", Arg.Set Flag.Print.abst_typ, " Print abstraction types when the program is safe";
     "-print-non-CPS-abst", Arg.Unit (fun () -> Flag.Mode.just_print_non_CPS_abst := true; Flag.Mode.trans_to_CPS := false), " Print non-CPS abstracted program (and exit)";
     "-print-as-ocaml", Arg.Set Flag.Print.as_ocaml, " Print terms in OCaml syntax";
     "-print-progress", Arg.Set Flag.Print.progress, " Print progress (use after -modular/-imodular)";
     "-print-unused-arg", Arg.Set Flag.Print.unused_arg, " Print unused arguments";
     "-print-cert", Arg.Set Flag.Print.certificate, " Print certificates even if the model checker does not support certificates (need TRecS)";
     (* preprocessing *)
     "", Arg.Unit ignore, "Options_for_preprocessing";
     "-fail-as-excep", Arg.Set Flag.Method.fail_as_exception, " Treat fail as an exception";
     "-replace-const", Arg.Set Flag.Method.replace_const, " Replace unchanging variables with constants";
     "-no-exparam", Arg.Set Flag.Method.no_exparam, " Do not add extra parameters";
     "-use-exparam", Arg.Clear Flag.Method.no_exparam, " Add extra parameters when CEGAR fails";
     "-list-option", Arg.Set Flag.Method.encode_list_opt, " Encode list using options not pairs";
     "-disable-preprocess", Arg.Clear Flag.Mode.init_trans, " Disable encoding of recursive data structures, CPS transformation, etc.";
     "-lift-fv", Arg.Set Flag.Method.lift_fv_only, " Lift variables which occur in a body";
     "-cps-naive", Arg.Set Flag.Method.cps_simpl, " Use naive CPS transformation";
     "-ins-param-funarg", Arg.Set Flag.Method.insert_param_funarg, " Insert an extra param for functions with function arguments";
     "-tupling", Arg.Unit (fun () -> Flag.Method.tupling := not !Flag.Method.tupling), " Toggle tupling";
     "-elim-same-arg", Arg.Set Flag.Method.elim_same_arg, " Eliminate same arguments";
     "-base-to-int", Arg.Set Flag.Method.base_to_int, " Replace primitive base types with int";
     (* verification *)
     "", Arg.Unit ignore, "Options_for_verifier";
     "-modular",
       Arg.Unit (fun () ->
                 Flag.Method.modular := true;
                 Flag.Print.modular_progress := !Flag.Print.progress;
                 Flag.Print.progress := false;
                 Flag.Modular.infer_ind := false),
       " Modular verification";
     "-imodular",
       Arg.Unit (fun () ->
                 Flag.Method.modular := true;
                 Flag.Print.modular_progress := !Flag.Print.progress;
                 Flag.Print.progress := false;
                 Flag.Modular.infer_ind := true),
       " Modular verification (inductive mode)";
     "-verify-ref-typ", Arg.Set Flag.Method.verify_ref_typ, " Verify functions have given refinement types";
     "-spec", Arg.Set_string Flag.spec_file, "<filename>  use <filename> as a specification";
     "-use-spec", Arg.Set Flag.Method.use_spec, " use XYZ.spec for verifying XYZ.ml if exists\n(This option is ignored if -spec is used)";
     "-disable-comment-spec", Arg.Clear Flag.Method.comment_spec, " disable {SPEC} on comments";
     "-module-verification", Arg.Set Flag.Mode.module_mode, " Check input as library";
     (* CEGAR *)
     "", Arg.Unit ignore, "Options_for_CEGAR";
     "-split-assert", Arg.Set Flag.Method.split_assert, " Reduce to verification of multiple programs\n(each program has only one assertion)";
     "-disable-predicate-accumulation", Arg.Set Flag.Refine.disable_predicate_accumulation, " Disable predicate accumulation";
     (* relatively complete verification *)
     "", Arg.Unit ignore, "Options_for_relatively_complete_verification";
     "-relative-complete", Arg.Set Flag.Method.relative_complete, " Enable relatively complete verification from the begining";
     (* predicate abstraction *)
     "", Arg.Unit ignore, "Options_for_predicate_abstraction";
     "-abs-remove-false", Arg.Set Flag.PredAbst.remove_false, " Do not use unsatisfiable predicates in abstraction";
     "-no-enr", Arg.Unit (fun _ -> Flag.PredAbst.expand_non_rec := false; Flag.PredAbst.expand_non_rec_init := false), " Do not expand non-recursive functions";
     "-enr", Arg.Unit (fun _ -> Flag.PredAbst.expand_non_rec := true; Flag.PredAbst.expand_non_rec_init := false),
             " Expand non-recursive functions except those in the original program";
     "-abs-filter", Arg.Set Flag.PredAbst.use_filter, " Turn on the abstraction-filter option";
     "-neg-pred-off", Arg.Set Flag.PredAbst.never_use_neg_pred,
                      " Never use negative predicates for abstraction";
     "-decomp-pred", Arg.Set Flag.PredAbst.decomp_pred, " Decompose abstraction predicates (e.g., [P1 && P2] ==> [P1, P2])";
     "-decomp-eq-pred", Arg.Set Flag.PredAbst.decomp_eq_pred, " Decompose abstraction predicates on equalities (e.g., [t1 = t2] ==> [t1 <= t2, t1 >= t2])";
     "-safe-fun-arg-pred-true",
       Arg.Unit (fun () -> Flag.PredAbst.assume_safe_fun_arg_pred_true := true; Fpat.AbsTypInfer.add_path_info := true),
       " Set predicates true for safe function arguments";
     "-non-cartesian", Arg.Clear Flag.PredAbst.cartesian, " Do non-cartesian abstraction";
     (* higher-order model checking *)
     "", Arg.Unit ignore, "Options_for_model_checking";
     "-rename-hors", Arg.Set Flag.ModelCheck.rename_hors, " Set different name to each hors file";
     "-ea", Arg.Set Flag.Print.eval_abst, " Print evaluation of abstacted program";
     "-bool-church", Arg.Set Flag.ModelCheck.church_encode, " Use church-encoding for model checking";
     "-trecs", Arg.Unit Flag.ModelCheck.(fun () -> mc:=TRecS), " Use TRecS as the model checker";
     "-horsat", Arg.Unit Flag.ModelCheck.(fun () -> mc:=HorSat), " Use HorSat as the model checker";
     "-horsat2", Arg.Unit Flag.ModelCheck.(fun () -> mc:=HorSat2), " Use HorSat2 as the model checker";
     "-trecs-bin", Arg.Set_string Flag.ModelCheck.trecs,
                   Format.sprintf "<cmd>  Change trecs command to <cmd> (default: \"%s\")" !Flag.ModelCheck.trecs;
     "-horsat-bin", Arg.Set_string Flag.ModelCheck.horsat,
                    Format.sprintf "<cmd>  Change horsat command to <cmd> (default: \"%s\")" !Flag.ModelCheck.horsat;
     "-horsat2-bin", Arg.Set_string Flag.ModelCheck.horsat2,
                    Format.sprintf "<cmd>  Change horsat2 command to <cmd> (default: \"%s\")" !Flag.ModelCheck.horsat2;
     "-horsatp-bin", Arg.Set_string Flag.ModelCheck.horsatp,
                     Format.sprintf "<cmd>  Change horsatp command to <cmd> (default: \"%s\")" !Flag.ModelCheck.horsatp;
     (* predicate discovery *)
     "", Arg.Unit ignore, "Options_for_predicate_discovery";
     "-fpat", Arg.String FpatInterface.parse_arg, "<option>  Pass <option> to FPAT";
     "-bool-init-empty", Arg.Set Flag.Method.bool_init_empty,
     " Use an empty set as the initial sets of predicates for booleans";
     "-mp", Arg.Set Flag.Refine.use_multiple_paths, " Use multiple infeasible error paths for predicate discovery";
     "-no-simplification", Arg.Set Flag.PredAbst.no_simplification, " Do not simplify abstracted programs";
     (* SWT solver *)
     "", Arg.Unit ignore, "Options_for_SMT_solver";
     "-cvc3-bin", Arg.Set_string Flag.cvc3,
                  Format.sprintf "<cmd>  Change cvc3 command to <cmd> (default: \"%s\")" !Flag.cvc3;
     (* fair termination mode *)
     "", Arg.Unit ignore, "Options_for_fair_termination_mode";
     "-fair-termination", Arg.Unit (fun _ -> Flag.Method.mode := Flag.Method.FairTermination), " Check fair termination";
     "-expand-set-flag", Arg.Set Flag.FairTermination.expand_set_flag, "";
     (* termination mode *)
     "-termination-disj",
       Arg.Unit (fun _ ->
                 Flag.Method.mode := Flag.Method.Termination;
                 Flag.Termination.disjunctive := true),
       " Check termination by finding disjunctive well-founded relation";
     "-termination",
       Arg.Unit (fun _ ->
                 Flag.Method.mode := Flag.Method.Termination),
       " Check termination";
     "-termination-sep",
       Arg.Unit (fun _ ->
                 Flag.Method.mode := Flag.Method.Termination;
                 Flag.Termination.separate_pred := true),
       " Check termination with separating {decrease, boundedness} verification";
     "-termination-split-callsite",
       Arg.Unit (fun _ ->
                 Flag.Method.mode := Flag.Method.Termination;
                 Flag.Termination.split_callsite := true),
       " Check termination for each callsite of functions";
     "-add-cd",
       Arg.Set Flag.Termination.add_closure_depth,
       " Insert extra parameters for representing depth of closures";
     "-infer-ranking-exparam",
       Arg.Set Flag.Termination.add_closure_exparam,
       " Infer extra ranking parameters for closures for termination verification";
     "-non-termination",
       Arg.Unit (fun _ ->
                   Flag.Method.mode := Flag.Method.NonTermination;
                   Flag.ModelCheck.church_encode := true;
                   Flag.ModelCheck.mc := Flag.ModelCheck.HorSat),
       " Check non-termination";
     (* non-termination mode *)
     "", Arg.Unit ignore, "Options_for_non-termination_mode";
     "-merge-paths",
       Arg.Set Flag.NonTermination.merge_paths_of_same_branch,
       " Merge predicates of paths that have same if-branch information";
     "-refinement-log",
       Arg.Set Flag.NonTermination.randint_refinement_log,
       " Write refinement types into log file (./refinement/[input file].refinement)";
     "-no-use-omega",
       Arg.Clear Flag.NonTermination.use_omega,
       " Do not use omega solver for under-approximation";
     "-use-omega-first",
       Arg.Set Flag.NonTermination.use_omega_first,
       " Preferentially use omega solver for under-approximation\n(if failed, we then check with z3)";
     (* fair non-termination mode *)
     "", Arg.Unit ignore, "Options_for_fair_non-termination_mode";
     "-fair-non-termination",
       Arg.Unit (fun _ ->
         Flag.Method.mode := Flag.Method.FairNonTermination;
         Flag.ModelCheck.church_encode := true;
         Flag.ModelCheck.mc := Flag.ModelCheck.HorSatP),
       " Check fair-non-termination";
     "-expand-ce-iter-init",
       Arg.Set_int Flag.FairNonTermination.expand_ce_iter_init,
       " Set the initial interaction count of counterexample expansion";
     "-expand-ce-count",
       Arg.Set_int Flag.FairNonTermination.expand_ce_iter_init,
       " Same as -expand-ce-iter-init";
     "", Arg.Unit ignore, "Other_options";
    ]
and print_option_and_exit () =
  !!arg_spec
  |> Arg.filter_out_desc
  |> List.map Triple.fst
  |> String.join " "
  |> print_string;
  exit 0
let arg_spec = arg_spec ()

let set_file name =
  let name' =
    match !Flag.pp with
    | None -> name
    | Some pp ->
        let name' = Filename.change_extension name "pml" in
        ignore @@ Sys.command @@ (Format.sprintf "%s %s -o '%s'" pp name name' |@> Format.printf "RUN: %s@.");
        name'
  in
  Flag.filenames := name' :: !Flag.filenames

let read_option_conf () =
  try
    let args =
      IO.CPS.open_in "option.conf" @@
        (input_line |- String.split_blanc)
    in
    Arg.current := 0;
    Arg.parse_argv (Array.of_list @@ Sys.argv.(0) :: args) arg_spec set_file usage;
    Flag.Log.args := !Flag.Log.args @ args
  with
  | Arg.Bad s
  | Arg.Help s -> Format.printf "%s@." s; exit 1
  | Sys_error _
  | End_of_file -> ()

let merge_input_files files =
  let filename = Filename.change_extension (List.hd files) "mml" in
  let cout = open_out filename in
  let ocf = Format.formatter_of_out_channel cout in
  let append file =
    let cin = open_in file in
    let s = IO.input_all cin in
    let module_name =
      file
      |> Filename.basename
      |> Filename.chop_extension_if_any
      |> String.capitalize
    in
    Format.fprintf ocf "module %s = struct@." module_name;
    Format.fprintf ocf "# 1 \"%s\"@." file;
    output_string cout s;
    Format.fprintf ocf "end@."
  in
  List.rev_iter append files;
  filename

let parse_arg () =
  Arg.parse arg_spec set_file usage;
  Flag.Log.args := Array.to_list Sys.argv;
  if not !Flag.Mode.ignore_conf then read_option_conf ();
  let filename =
    match !Flag.filenames with
    | []
    | ["-"] ->
        let filename = "stdin.ml" in
        Flag.filenames := [filename];
        IO.output_file filename (IO.input_all stdin);
        filename
    | _ ->
        let filename =
          match !Flag.filenames with
          | [] -> assert false
          | [file] -> file
          | files -> merge_input_files files
        in
        Config.load_path := Filename.dirname !!Flag.mainfile :: !Config.load_path;
        filename
  in
  let cin = open_in filename in
  Flag.Method.input_cegar := String.ends_with !!Flag.mainfile ".cegar";
  cin

(* called before parsing options *)
let fpat_init1 () =
  Fpat.FPATConfig.set_default ()

(* called after parsing options *)
let fpat_init2 () =
  let open Fpat in
  Global.target_filename := !!Flag.mainfile;
  SMTProver.cvc3_command := !Flag.cvc3;
  SMTProver.initialize ()

let check_env () =
  match !Flag.ModelCheck.mc with
  | Flag.ModelCheck.TRecS -> if not Environment.trecs_available then fatal "TRecS not found"
  | Flag.ModelCheck.HorSat -> if not Environment.horsat_available then fatal "HorSat not found"
  | Flag.ModelCheck.HorSat2 -> if not Environment.horsat2_available then fatal "HorSat2 not found"
  | Flag.ModelCheck.HorSatP -> if not Environment.horsatp_available then fatal "HorSatP not found"

let init_after_parse_arg () =
  if Flag.ModelCheck.(!mc <> TRecS) then
    Flag.ModelCheck.church_encode := true

let timeout_handler _ = raise TimeOut

let string_of_exception = function
  | e when Fpat.FPATConfig.is_fpat_exception e ->
     Fpat.FPATConfig.string_of_fpat_exception e
  | Syntaxerr.Error err -> "Syntaxerr.Error"
  | Typecore.Error(loc,env,err) -> "Typecore.Error"
  | Typemod.Error(loc,env,err) -> "Typemod.Error"
  | Env.Error e -> "Env.Error"
  | Typetexp.Error(loc,env,err) -> "Typetexp.Error"
  | Lexer.Error(err, loc) -> "Lexer.Error"
  | CEGAR_syntax.NoProgress -> "CEGAR_syntax.NoProgress"
  | Fatal s -> "Fatal"
  | TimeOut
  | Assert_failure("timer.ml", _, _) -> "TimeOut"
  | Killed -> "Killed"
  | Z3native.Exception("out of memory") -> "OutOfMemory"
  | e -> Printexc.to_string e

let print_error = function
  | Fpat.RefTypInfer.FailedToRefineTypes ->
      Format.printf "Verification failed:@.";
      Format.printf "  MoCHi could not refute an infeasible error path @.";
      Format.printf "  due to the incompleteness of the refinement type system@."
  | e when Fpat.FPATConfig.is_fpat_exception e ->
      Format.printf "FPAT: %a@." Fpat.FPATConfig.pr_exception e
  | Syntaxerr.Error err ->
      Format.printf "%a@." Syntaxerr.report_error err
  | Typecore.Error(loc,env,err) ->
      Format.printf "%a%a@." Location.print_error loc (Typecore.report_error env) err
  | Typemod.Error(loc,env,err) ->
      Format.printf "%a%a@." Location.print_error loc (Typemod.report_error env) err
  | Env.Error e ->
      Format.printf "%a@." Env.report_error e
  | Typetexp.Error(loc,env,err) ->
      Format.printf "%a%a@." Location.print_error loc (Typetexp.report_error env) err
  | Lexer.Error(err, loc) ->
      Format.printf "%a%a@." Location.print_error loc Lexer.report_error err
  | CEGAR_syntax.NoProgress ->
      Format.printf "Unknown. (CEGAR not progress) @."
  | CEGAR_abst.NotRefined ->
      Format.printf "Verification failed (new error path not found)@."
  | Fatal s ->
      Format.printf "Fatal error: %s@." s
  | Unsupported s ->
      Format.printf "Unsupported: %s@." s
  | Sys_error s ->
      Format.printf "%s@." s
  | TimeOut
  | Assert_failure("timer.ml", _, _) ->
      Format.printf "Verification failed (time out)@."
  | Z3native.Exception("out of memory") ->
      Format.printf "Verification failed (out of memory)@."
  | e when !Flag.Debug.debug_module = [] ->
      Format.printf "Exception: %s@." @@ Printexc.to_string e
  | e -> raise e


let () =
  if !Sys.interactive
  then ()
  else
    try
      fpat_init1 ();
      let cin = parse_arg () in
      ignore @@ Unix.alarm !Flag.time_limit;
      fpat_init2 ();
      Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise TimeOut));
      Color.init ();
      if not !!is_only_result then print_env true false;
      init_after_parse_arg ();
      check_env ();
      if main cin then decr Flag.Log.cegar_loop;
      Fpat.SMTProver.finalize ();
      print_info ()
    with
    | e when !Flag.Debug.debug_module = [] ->
        Flag.Log.result := string_of_exception e;
        Format.print_flush ();
        flush_all ();
        Option.iter output_csv !Flag.Log.output_csv;
        Option.iter output_json !Flag.Log.output_json;
        Main_loop.print_result_delimiter ();
        if not !!is_only_result then print_error e;
        exit 1
