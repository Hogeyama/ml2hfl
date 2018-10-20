open Util

let print_info_default () =
  if !Flag.Termination.add_closure_exparam && !Flag.Log.result = "terminating" then
    Format.printf "exparam inserted program:@. %a@." Print.(term_custom {!config_default with unused=true}) !ExtraParamInfer.origWithExparam;
  if Flag.Method.(!mode = Termination) && !Flag.Log.result = "terminating" then
    begin
      List.iter
        (fun (f_name, (cycles, pred)) ->
         Format.printf "ranking function(%s)[inference cycle: %d]: %a\n" f_name cycles BRA_types.pr_ranking_function pred;
         if !Flag.Termination.add_closure_exparam then
           let str_exparam = ExtraParamInfer.to_string_CoeffInfos pred.BRA_types.coeffsMap in
           if str_exparam <> "" then Format.printf "exparam(%s):\n%s\n" f_name str_exparam)
        !Termination_loop.lrf
    end;
  if Flag.Method.(!mode = FairTermination) then
    Format.printf "cycles: %d@." !Flag.FairTermination.loop_count;
  Format.printf "CEGAR-cycles: %d@." !Flag.Log.cegar_loop;
  Format.printf "total: %.3f sec@." !!Time.get;
  Format.printf "  abst: %.3f sec@." !Flag.Log.Time.abstraction;
  Format.printf "  mc: %.3f sec@." !Flag.Log.Time.mc;
  if Flag.Experiment.HORS_quickcheck.(!use <> None) then
    Format.printf "    hors_quickcheck: %.3f sec@." !Flag.Log.Time.hors_quickcheck;
  Format.printf "  refine: %.3f sec@." !Flag.Log.Time.cegar;
  if !Flag.Method.relative_complete then
    Format.printf "    exparam: %.3f sec@." !Flag.Log.Time.parameter_inference;
  Format.pp_print_flush Format.std_formatter ()

let output_csv filename =
  let oc = open_out_gen [Open_append; Open_creat] 0o644 filename in
  let pr fmt = Printf.fprintf oc fmt in
  let pr_mod fmt = if !Flag.Method.modular then Printf.fprintf oc fmt else Printf.ifprintf oc fmt in
  pr "%s," @@ Filename.chop_extension_if_any @@ Filename.basename !Flag.mainfile;
  pr "%S," !Flag.Log.result;
  pr "%d," !Flag.Log.cegar_loop;
  pr "%f," !!Time.get;
  pr "%f," !Flag.Log.Time.abstraction;
  pr "%f," !Flag.Log.Time.mc;
  pr "%f," !Flag.Log.Time.cegar;
  pr "%f," !Flag.Log.Time.parameter_inference;
  pr_mod "%n," !Modular.num_tycheck;
  pr_mod "%f," !Modular.time_check;
  pr_mod "%f," !Modular.time_synthesize;
  pr "0\n";
  close_out oc

let output_json filename =
  let oc =
    if filename = "-" then
      stdout
    else
      open_out_gen [Open_append; Open_creat] 0o644 filename
  in
  let pr fmt = Printf.fprintf oc fmt in
  let pr_ter fmt = if Flag.Method.(!mode = Termination) then Printf.fprintf oc fmt else Printf.ifprintf oc fmt in
  let pr_mod fmt = if !Flag.Method.modular then Printf.fprintf oc fmt else Printf.ifprintf oc fmt in
  pr "{\"filename\": %S" !Flag.mainfile;
  pr ", \"result\": %S" !Flag.Log.result;
  pr ", \"cycles\": %d" !Flag.Log.cegar_loop;
  pr_ter ", \"ranking\": {";
  let rec pr_rfs rfs =
    let pr_rf (f_name, (cycles, pred)) =
      let rank_fun = Format.asprintf "%a" BRA_types.pr_ranking_function pred in
      pr_ter "%S: {\"function\": %S, \"inferCycles\": %d}" f_name rank_fun cycles
    in
    match rfs with
    | [] -> ()
    | [rf] -> pr_rf rf
    | rf::rfs' -> pr_rf rf; pr_ter ", "; pr_rfs rfs'
  in
  pr_rfs !Termination_loop.lrf;
  pr_ter "}";
  pr ", \"total\": %f" !!Time.get;
  pr ", \"abst\": %f" !Flag.Log.Time.abstraction;
  pr ", \"mc\": %f" !Flag.Log.Time.mc;
  if Flag.Experiment.HORS_quickcheck.(!use <> None) then
    begin
      pr ", \"hors_quickcheck\": %f" !Flag.Log.Time.hors_quickcheck;
      pr ", \"cex_length\": %s" @@ List.to_string ~delimiter:"," string_of_int !Flag.Experiment.HORS_quickcheck.cex_length_history
    end;
  pr ", \"refine\": %f" !Flag.Log.Time.cegar;
  if !Flag.Method.relative_complete then
    pr ", \"exparam\": %f" !Flag.Log.Time.parameter_inference;
  pr_mod ", \"#typeChecker\": %d" !Modular.num_tycheck;
  pr_mod ", \"typeChecker\": %f" !Modular.time_check;
  pr_mod ", \"typeSynthesizer\": %f" !Modular.time_synthesize;
  pr "}\n"

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
  let z3_lib =
    let a,b,c,d = Z3native.get_version () in
    Format.sprintf "%d.%d.%d.%d" a b c d
  in
  let z3_bin = if Mconfig.z3_available then Some (String.trim @@ Unix.CPS.open_process_in (Mconfig.z3 ^ " -version") IO.input_all) else None in
  let trecs = TrecsInterface.version () in
  let horsat = HorSatInterface.version () in
  let horsat2 = HorSat2Interface.version () in
  let horsatp = HorSatPInterface.version () in
  if json then
    try
      Option.iter (Format.printf "{Build:%S," -| fst) mochi;
      Format.printf "\"Z3 library\":%S," z3_lib;
      Option.iter (Format.printf "\"Z3 binary\":%S,") z3_bin;
      Option.iter (Format.printf "TRecS:%S,") trecs;
      Option.iter (Format.printf "HorSat:%S,") horsat;
      Option.iter (Format.printf "HorSat2:%S,") horsat2;
      Option.iter (Format.printf "HorSatP:%S,") horsatp;
      Format.printf "OCaml:%S}" Sys.ocaml_version;
    with Option.No_value -> exit 1
  else
    begin
      Color.printf Color.Green "MoCHi: Model Checker for Higher-Order Problems@.";
      Option.iter (fun (r,t) -> Format.printf "  Build: %s (%s)@." r t) mochi;
      Format.printf "  Z3 library version: %s@." z3_lib;
      Option.iter (Format.printf "  Z3 binary: %s@.") z3_bin;
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
  let coeffs = List.filter Id.is_coefficient @@ Term_util.get_fv parsed in
  let result =
    try
      List.for_all
        (fun holed ->
         let init_predicate_info =
           { BRA_types.variables = List.map BRA_transform.extract_id (BRA_state.get_argvars holed.BRA_types.state holed.BRA_types.verified)
           ; BRA_types.coeffsMap = if !Flag.Termination.add_closure_exparam then List.map (Pair.add_right @@ Fun.const 0) coeffs else []
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
    begin
      Flag.Log.result := "terminating";
      if !Flag.Print.result then Format.printf "Terminating!@."
    end
  else
   begin
     Flag.Log.result := "unknown";
     if !Flag.Print.result then Format.printf "Unknown...@."
   end;
  result

let main_fair_termination orig spec parsed =
  let result = Fair_termination.run spec parsed in
  if result
  then Format.printf "Fair terminating!@.@."
  else Format.printf "Unknown...@.@.";
  result

let output_randint_refinement_log input_string =
  let cout =
    let input =
      let dirname = Filename.dirname !Flag.mainfile in
      let basename = Filename.basename !Flag.mainfile in
      dirname ^ "/refinement/" ^ Filename.change_extension basename "refinement"
    in
    open_out_gen [Open_wronly; Open_trunc; Open_text; Open_creat] 0o644 input
  in
  output_string cout ("[INPUT]:\n" ^ input_string ^ "\n");
  close_out cout

let main_quick_check spec t =
  t
  |> Preprocess.(run_on_term (before CPS @@ all spec))
  |> Preprocess.get
  |> Quick_check.repeat_forever

let main_trans spec t =
  let pps_all = Preprocess.all spec in
  let print_as_ml pps =
       Preprocess.run_on_term pps
    |- Preprocess.get
    |- Trans.remove_unambiguous_id
    |- Trans.replace_typ_result_with_unit
    |- Trans.rename_for_ocaml
    |- Format.printf "%a@." Print.as_ocaml_typ
  in
  begin
    match !Flag.Trans.destination with
    | Flag.Trans.Before_CPS -> print_as_ml Preprocess.(before CPS pps_all) t
    | Flag.Trans.CPS -> print_as_ml Preprocess.(before_and CPS pps_all) t
    | Flag.Trans.CHC ->
        Flag.PredAbst.shift_pred := true;
        let t' =
          t
          |> Preprocess.(run_on_term (before_and CPS pps_all))
          |> Preprocess.get
          |> Trans.alpha_rename ~whole:true
        in
        let ty = Ref_type.of_simple t'.Syntax.typ in
        let env = Ref_type.Env.empty in
        try
          Ref_type_check.print stdout env t' ty
        with e when !Flag.Debug.debug_module = [] ->
          Format.eprintf "%s@." (Printexc.to_string e);
          Format.printf ")@.(get-proof)@."; (* for hoice *)
          exit 1
  end;
  exit 0

let main cin =
  let input_string =
    let s = IO.input_all cin in
    if Flag.Method.(!mode = FairTermination || !mode = FairNonTermination)
    then Fair_termination_util.add_event s
    else s
  in
  let lb = Lexing.from_string input_string in
  lb.Lexing.lex_curr_p <-
    {Lexing.pos_fname = Filename.basename !Flag.mainfile;
     Lexing.pos_lnum = 1;
     Lexing.pos_cnum = 0;
     Lexing.pos_bol = 0};
  if !Flag.Method.input_cegar then
    main_input_cegar lb
  else
    let orig = Parse.use_file lb in
    let parsed = Parser_wrapper.from_use_file orig in
    Verbose.printf "%a:@. @[%a@.@." Color.s_red "parsed" Print.term_typ parsed;
    if !Flag.NonTermination.randint_refinement_log then output_randint_refinement_log input_string;
    let spec = Spec.read Spec_parser.spec Spec_lexer.token |@> Verbose.printf "%a@." Spec.print in
    (* TODO: Refactor below *)
    let verify t =
      if !Flag.Method.modular then
        Modular.main orig spec t
      else
        let env_assume = Spec.get_ext_ref_env spec t in
        let env_assert = Spec.get_ref_env spec t in
        let problem =
          if env_assert = [] then
            Problem.safety ~env:env_assume t
          else
            Problem.ref_type_check ~env:env_assume t env_assert
        in
        Main_loop.run orig ~spec problem
    in
    if Flag.Method.(!mode = Trans) then
        main_trans spec parsed
    else if !Flag.Method.quick_check then
      main_quick_check spec parsed
    else if !Flag.Method.verify_ref_typ then
      Verify_ref_typ.main orig spec parsed
    else if Flag.Method.(!mode = Termination) then
      main_termination orig parsed
    else if Flag.Method.(!mode = FairTermination) then
      main_fair_termination orig spec parsed
    else if !Flag.Mode.module_mode then
      Verify_module.main verify parsed
    else
      verify parsed


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

let just_run_other_command cmd =
  if !Flag.filenames = [] then
    (Format.eprintf "Option \"-just-run\" must follow input file@."; exit 1);
  let filename = List.hd !Flag.filenames in
  let total,r = Time.measure (fun () -> Sys.command @@ snd @@ String.replace ~str:cmd ~sub:"%i" ~by:filename) in
  let result = if r = 0 then "Safe" else "Error" in
  Format.printf "{filename:%S, result:%S, total:%f}@." filename result total;
  exit r

let usage =
  "MoCHi: Model Checker for Higher-Order Problems\n\n" ^
    "Usage: " ^ Sys.executable_name ^ " [options] file\noptions are:"
let rec arg_spec () =
  Arg.align
    ["-I", Arg.String (fun dir -> Config.load_path := dir::!Config.load_path),
     "<dir>  Add <dir> to the list of include directories";
     "-margin", Arg.Int Format.set_margin, "<n>  Set pretty printing margin";
     "-only-result", Arg.Unit set_only_result, " Show only result";
     "-debug", Arg.String Flag.Debug.set_debug_modules, "<modules>  Set debug flag of modules (comma-separated)";
     "-color", Arg.Set Flag.PrettyPrinter.color, " Turn on syntax highlighting";
     "-color-always", Arg.Set Flag.PrettyPrinter.color_always, " Turn on syntax highlighting even if stdout does not refer to a terminal";
     "-ignore-conf", Arg.Set Flag.Mode.ignore_conf, " Ignore option.conf";
     "-v", Arg.Unit (fun () -> print_env false false; exit 0), " Print the version shortly";
     "-env", Arg.Unit (fun () -> print_env false true; exit 0), " Print the version and the environment as JSON";
     "-version", Arg.Unit (fun () -> print_env false false; exit 0), " Print the version";
     "-limit", Arg.Set_int Flag.time_limit, " Set time limit (seconds)";
     "-pp", Arg.String (fun pp -> Flag.pp := Some pp), " Set preprocessor command";
     "-web", Arg.Set Flag.PrettyPrinter.web, " Web mode";
     "-rand-self-init", Arg.Unit Random.self_init, " Initialize the random seed";
     "-use-temp", Arg.Set Flag.use_temp, " Use temporary files for intermediate/log files";
     "-trans",
       Arg.String Flag.(fun s -> Method.(mode := Trans);
                                 Trans.set_trans s;
                                 set_only_result ()),
       Format.asprintf "<desc>  Translate the input to <dest> which must be one of the following:\n%s"
                       !!Flag.Trans.string_of_destinations;
     (* experiment *)
     "", Arg.Unit ignore, "Options_for_experiments";
     "-exp", Arg.String set_exp_filename, "<filename>  Output experimental results to <filename> (Support file types: *.csv, *.json)";
     "-exp-json", Arg.Unit (fun () -> set_exp_filename "-"), " Output experimental results to starndard output as JSON";
     "-just-run", Arg.String just_run_other_command, " (just for experiments, %i is replaced with the filename)";
     "-hors-quickcheck-short", Arg.Unit Flag.(fun () -> Experiment.HORS_quickcheck.(use =: Some Shortest)), " Use shortest counterexample generated by hors_quickcheck";
     "-hors-quickcheck-long", Arg.Unit Flag.(fun () -> Experiment.HORS_quickcheck.(use =: Some Longest)), " Use longest counterexample generated by hors_quickcheck";
     "-hors-quickcheck-low", Arg.Unit Flag.(fun () -> Experiment.HORS_quickcheck.(use =: Some LowestCoverage)), " Use lowest coverage counterexample generated by hors_quickcheck";
     "-hors-quickcheck-high", Arg.Unit Flag.(fun () -> Experiment.HORS_quickcheck.(use =: Some HighestCoverage)), " Use highest coverage counterexample generated by hors_quickcheck";
     (* abstraction *)
     "", Arg.Unit ignore, "Options_for_abstraction";
     "-ignore-exn-arg", Arg.Unit Flag.(fun () -> Method.ignore_exn_arg =: true), " Ignore exception arguments";
     "-abst-literal", Arg.Int Flag.(fun n -> Method.abst_literal =: n), " Abstract literals";
     "-abst-list-eq", Arg.Unit Flag.(fun () -> Method.abst_list_eq =: true), " Abstract list equalities";
     "-ignore-non-termination", Arg.Unit Flag.(fun () -> Method.ignore_non_termination =: true), " Ignore non-termination";
     (* completion *)
     "", Arg.Unit ignore, "Options_for_completion";
     "-option-list", Arg.Unit print_option_and_exit, " Print list of options";
     "-debug-list", Arg.Unit (fun () -> List.iter (Format.printf "%s@.") !Flag.Debug.debuggable_modules; exit 0), " Print list of debug options";
     "-trans-list", Arg.Unit (fun () -> List.iter (Format.printf "%s@.") @@ List.map fst Flag.Trans.destinations; exit 0), " Print list of -trans destinations";
     (* printing *)
     "", Arg.Unit ignore, "Options_for_printing";
     "-print-abst-types", Arg.Set Flag.Print.abst_typ, " Print abstraction types when the program is safe";
     "-print-non-CPS-abst", Arg.Unit Flag.(fun () -> Mode.just_print_non_CPS_abst := true; Flag.Mode.trans_to_CPS := false), " Print non-CPS abstracted program (and exit)";
     "-print-as-ocaml", Arg.Unit Print.set_print_as_ocaml, " Print terms in OCaml syntax";
     "-print-progress", Arg.Set Flag.Print.progress, " Print progress (use after -modular/-imodular)";
     "-print-unused-arg", Arg.Unit Print.set_print_unused, " Print unused arguments";
     "-print-cert", Arg.Set Flag.Print.certificate, " Print certificates even if the model checker does not support certificates (need TRecS)";
     (* preprocessing *)
     "", Arg.Unit ignore, "Options_for_preprocessing";
     "-fail-as-excep", Arg.Unit Flag.(fun () -> Method.fail_as_exception =: true), " Treat fail as an exception";
     "-replace-const", Arg.Unit Flag.(fun () -> Method.replace_const =: true), " Replace unchanging variables with constants";
     "-no-exparam", Arg.Unit Flag.(fun () -> Method.no_exparam =: true), " Do not add extra parameters";
     "-use-exparam", Arg.Unit Flag.(fun () -> Method.no_exparam =: false), " Add extra parameters when CEGAR fails";
     "-list-option", Arg.Unit Flag.(fun () -> Method.encode_list_opt =: true), " Encode list using options not pairs";
     "-disable-preprocess", Arg.Unit Flag.(fun () -> Mode.init_trans =: false), " Disable encoding of recursive data structures, CPS transformation, etc.";
     "-lift-fv", Arg.Unit Flag.(fun () -> Method.lift_fv_only =: true), " Lift variables which occur in a body";
     "-cps-naive", Arg.Unit Flag.(fun () -> Method.cps_simpl =: true), " Use naive CPS transformation";
     "-ins-param-funarg", Arg.Unit Flag.(fun () -> Method.insert_param_funarg =: true), " Insert an extra param for functions with function arguments";
     "-tupling", Arg.Unit Flag.(fun () -> Method.tupling =: not !Flag.Method.tupling), " Toggle tupling";
     "-elim-same-arg", Arg.Unit Flag.(fun () -> Method.elim_same_arg =: true), " Eliminate same arguments";
     "-base-to-int", Arg.Unit Flag.(fun () -> Method.base_to_int =: true), " Replace primitive base types with int";
     "-data-to-int", Arg.Unit Flag.(fun () -> Method.data_to_int =: true), " Replace data types with int";
     "-bool-to-int", Arg.Unit Flag.(fun () -> Method.bool_to_int =: true), " Encode booleans into integers";
     (* verification *)
     "", Arg.Unit ignore, "Options_for_verification";
     "-modular",
       Arg.Unit Flag.(fun () ->
                      Method.modular =: true;
                      Print.modular_progress := !Flag.Print.progress;
                      Print.progress := false;
                      Modular.infer_ind =: false),
       " Modular verification";
     "-imodular",
       Arg.Unit Flag.(fun () ->
                      Method.modular =: true;
                      Print.modular_progress := !Flag.Print.progress;
                      Print.progress := false;
                      Modular.infer_ind =: true),
       " Modular verification (inductive mode)";
     "-verify-ref-typ", Arg.Unit Flag.(fun () -> Method.verify_ref_typ =: true), " Verify functions have given refinement types";
     "-spec", Arg.Set_string Flag.spec_file, "<filename>  use <filename> as a specification";
     "-use-spec", Arg.Set Flag.Method.use_spec, " use XYZ.spec for verifying XYZ.ml if exists\n(This option is ignored if -spec is used)";
     "-disable-comment-spec", Arg.Clear Flag.Method.comment_spec, " disable {SPEC} on comments";
     "-module-verification", Arg.Set Flag.Mode.module_mode, " Check input as library";
     "-quickcheck", Arg.Unit Flag.(fun () -> Method.quick_check =: true), " Disprove safety via QuickCheck (other method options will be ignored)";
     (* Modular verification *)
     "", Arg.Unit ignore, "Options_for_Modular_verification";
     "-check-simple", Arg.Unit Flag.(fun () -> Modular.check_simple =: true), " Use simple refinement checking";
     (* CEGAR *)
     "", Arg.Unit ignore, "Options_for_CEGAR";
     "-split-assert", Arg.Unit Flag.(fun () -> Method.split_assert =: true), " Reduce to verification of multiple programs\n(each program has only one assertion)";
     "-disable-predicate-accumulation", Arg.Unit Flag.(fun () -> Refine.disable_predicate_accumulation =: true), " Disable predicate accumulation";
     (* relatively complete verification *)
     "", Arg.Unit ignore, "Options_for_relatively_complete_verification";
     "-relative-complete", Arg.Unit Flag.(fun () -> Method.relative_complete =: true), " Enable relatively complete verification from the begining";
     (* predicate abstraction *)
     "", Arg.Unit ignore, "Options_for_predicate_abstraction";
     "-abs-remove-false", Arg.Unit Flag.(fun () -> PredAbst.remove_false =: true), " Do not use unsatisfiable predicates in abstraction";
     "-no-enr", Arg.Unit Flag.(fun () -> PredAbst.expand_non_rec =: false; Flag.PredAbst.expand_non_rec_init =: false), " Do not expand non-recursive functions";
     "-enr", Arg.Unit Flag.(fun () -> PredAbst.expand_non_rec =: true; Flag.PredAbst.expand_non_rec_init =: false),
             " Expand non-recursive functions except those in the original program";
     "-abs-filter", Arg.Unit Flag.(fun () -> PredAbst.use_filter =: true), " Turn on the abstraction-filter option";
     "-neg-pred-off", Arg.Unit Flag.(fun () -> PredAbst.never_use_neg_pred =: true),
                      " Never use negative predicates for abstraction";
     "-decomp-pred", Arg.Unit Flag.(fun () -> PredAbst.decomp_pred =: true), " Decompose abstraction predicates (e.g., [P1 && P2] ==> [P1, P2])";
     "-decomp-eq-pred", Arg.Unit Flag.(fun () -> PredAbst.decomp_eq_pred =: true), " Decompose abstraction predicates on equalities (e.g., [t1 = t2] ==> [t1 <= t2, t1 >= t2])";
     "-shift-pred", Arg.Unit Flag.(fun () -> PredAbst.shift_pred =: true), " Set predicates true for safe function arguments";
     "-non-cartesian", Arg.Unit Flag.(fun () -> PredAbst.cartesian =: false), " Do non-cartesian abstraction";
     (* higher-order model checking *)
     "", Arg.Unit ignore, "Options_for_model_checking";
     "-rename-hors", Arg.Unit Flag.(fun () -> ModelCheck.rename_hors =: true), " Set different name to each hors file";
     "-ea", Arg.Set Flag.Print.eval_abst, " Print evaluation of abstacted program";
     "-bool-church", Arg.Unit Flag.(fun () -> ModelCheck.church_encode =: true), " Use church-encoding for model checking";
     "-trecs", Arg.Unit Flag.(fun () -> ModelCheck.(mc =: TRecS)), " Use TRecS as the model checker";
     "-horsat", Arg.Unit Flag.(fun () -> ModelCheck.(mc =: HorSat)), " Use HorSat as the model checker";
     "-horsat2", Arg.Unit Flag.(fun () -> ModelCheck.(mc =: HorSat2)), " Use HorSat2 as the model checker";
     "-trecs-bin", Arg.String Flag.(fun s -> ModelCheck.trecs =: s),
                   Format.sprintf "<cmd>  Change trecs command to <cmd> (default: \"%s\")" !Flag.ModelCheck.trecs;
     "-horsat-bin", Arg.String Flag.(fun s -> ModelCheck.horsat =: s),
                    Format.sprintf "<cmd>  Change horsat command to <cmd> (default: \"%s\")" !Flag.ModelCheck.horsat;
     "-horsat2-bin", Arg.String Flag.(fun s -> ModelCheck.horsat2 =: s),
                    Format.sprintf "<cmd>  Change horsat2 command to <cmd> (default: \"%s\")" !Flag.ModelCheck.horsat2;
     "-horsatp-bin", Arg.String Flag.(fun s -> ModelCheck.horsatp =: s),
                     Format.sprintf "<cmd>  Change horsatp command to <cmd> (default: \"%s\")" !Flag.ModelCheck.horsatp;
     (* predicate discovery *)
     "", Arg.Unit ignore, "Options_for_predicate_discovery";
     "-fpat", Arg.String FpatInterface.parse_arg, "<option>  Pass <option> to FPAT";
     "-bool-init-empty", Arg.Unit Flag.(fun () -> Method.bool_init_empty =: true),
     " Use an empty set as the initial sets of predicates for booleans";
     "-mp", Arg.Unit Flag.(fun () -> Refine.use_multiple_paths =: true), " Use multiple infeasible error paths for predicate discovery";
     "-no-simplification", Arg.Unit Flag.(fun () -> PredAbst.no_simplification =: true), " Do not simplify abstracted programs";
     "-rec-chc", Arg.Unit Flag.(fun () -> Refine.use_rec_chc_solver =: true), " Use recursive CHC solver";
     "-rec-chc-limit", Arg.Int Flag.(fun n -> Refine.solver_timelimit =: n), " Set time limit for recursive CHC solver (seconds)";
     "-hoice", Arg.Unit Flag.(fun () -> Refine.(solver =: Hoice)), " Use HoICE as the recursive horn-clause solver";
     "-hoice-bin", Arg.String Flag.(fun s -> Refine.hoice =: s),
                   Format.sprintf "<cmd>  Change hoice command to <cmd> (default: \"%s\")" !Flag.Refine.hoice;
     "-z3", Arg.Unit Flag.(fun () -> Refine.(solver =: Z3)), " Use Z3 as the recursive horn-clause solver";
     "-z3-bin", Arg.String Flag.(fun s -> Refine.z3 =: s),
                Format.sprintf "<cmd>  Change z3 command to <cmd> (default: \"%s\")" !Flag.Refine.z3;
     "-z3-spacer", Arg.Unit Flag.(fun () -> Refine.(solver =: Z3_spacer)), " Use Z3 (Spacer) as the recursive horn-clause solver";
     "-z3-spacer-bin", Arg.String Flag.(fun s -> Refine.z3_spacer =: s),
                   Format.sprintf "<cmd>  Change z3-spacer command to <cmd> (default: \"%s\")" !Flag.Refine.z3_spacer;
     (* SWT solver *)
     "", Arg.Unit ignore, "Options_for_SMT_solver";
     "-cvc3-bin", Arg.String Flag.(fun s -> cvc3 =: s),
                  Format.sprintf "<cmd>  Change cvc3 command to <cmd> (default: \"%s\")" !Flag.cvc3;
     (* fair termination mode *)
     "", Arg.Unit ignore, "Options_for_fair_termination_mode";
     "-fair-termination", Arg.Unit Flag.(fun () -> Method.mode =: Flag.Method.FairTermination), " Check fair termination";
     "-expand-set-flag", Arg.Unit Flag.(fun () -> FairTermination.expand_set_flag =: true), "";
     (* termination mode *)
     "-termination-disj",
       Arg.Unit Flag.(fun _ ->
                      Method.mode =: Method.Termination;
                      Termination.disjunctive =: true),
       " Check termination by finding disjunctive well-founded relation";
     "-termination",
       Arg.Unit Flag.(fun _ -> Method.(mode =: Termination)),
       " Check termination";
     "-termination-sep",
       Arg.Unit Flag.(fun _ ->
                      Method.mode =: Method.Termination;
                      Termination.separate_pred =: true),
       " Check termination with separating {decrease, boundedness} verification";
     "-termination-split-callsite",
       Arg.Unit Flag.(fun _ ->
                      Method.mode =: Method.Termination;
                      Termination.split_callsite =: true),
       " Check termination for each callsite of functions";
     "-add-cd",
       Arg.Unit Flag.(fun () -> Termination.add_closure_depth =: true),
       " Insert extra parameters for representing depth of closures";
     "-infer-ranking-exparam",
       Arg.Unit Flag.(fun () -> Termination.add_closure_exparam =: true),
       " Infer extra ranking parameters for closures for termination verification";
     "-non-termination",
       Arg.Unit Flag.(fun _ ->
                      Method.mode =: Method.NonTermination;
                      ModelCheck.church_encode =: true;
                      ModelCheck.mc =: ModelCheck.HorSat),
       " Check non-termination";
     (* non-termination mode *)
     "", Arg.Unit ignore, "Options_for_non-termination_mode";
     "-merge-paths",
       Arg.Unit Flag.(fun () -> NonTermination.merge_paths_of_same_branch =: true),
       " Merge predicates of paths that have same if-branch information";
     "-refinement-log",
       Arg.Unit Flag.(fun () -> NonTermination.randint_refinement_log =: true),
       " Write refinement types into log file (./refinement/[input file].refinement)";
     "-no-use-omega",
       Arg.Unit Flag.(fun () -> NonTermination.use_omega =: false),
       " Do not use omega solver for under-approximation";
     "-use-omega-first",
       Arg.Unit Flag.(fun () -> NonTermination.use_omega_first =: true),
       " Preferentially use omega solver for under-approximation\n(if failed, we then check with z3)";
     (* fair non-termination mode *)
     "", Arg.Unit ignore, "Options_for_fair_non-termination_mode";
     "-fair-non-termination",
       Arg.Unit Flag.(fun _ ->
                      Method.mode =: Method.FairNonTermination;
                      ModelCheck.church_encode =: true;
                      ModelCheck.mc =: ModelCheck.HorSatP),
       " Check fair-non-termination";
     "-expand-ce-iter-init",
       Arg.Int Flag.(fun n -> FairNonTermination.expand_ce_iter_init =: n),
       " Set the initial interaction count of counterexample expansion";
     "-expand-ce-count",
       Arg.Int Flag.(fun n -> FairNonTermination.expand_ce_iter_init =: n),
       " Same as -expand-ce-iter-init";
     "", Arg.Unit ignore, "Other_options";
    ]
and print_option_and_exit () =
  !!arg_spec
  |> Arg.filter_out_desc
  |> List.map Triple.fst
  |> List.iter @@ Format.printf "%s@.";
  exit 0
let arg_spec = arg_spec ()

let set_file name =
  let name' =
    match !Flag.pp with
    | None -> name
    | Some pp ->
        let name' = Filename.change_extension name "pml" in
        ignore @@ Sys.command @@ Format.sprintf "%s %s -o '%s'" pp name name';
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
  | Arg.Help s -> Format.eprintf "%s@." s; exit 1
  | Sys_error _
  | End_of_file -> ()

let make_temp_file () =
  let dir = "/tmp/mochi" in
  let template = Format.asprintf "%s/%a_XXXXXXXX.ml" dir Time.print_simple !!Unix.time in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error(Unix.EEXIST, _, _) -> ());
  Unix.CPS.open_process_in ("mktemp " ^ template) input_line
  |@> Verbose.printf "Temporary file \"%s\" is created@.@."

let copy_input_file file =
  let temp_file = !!make_temp_file in
  IO.copy_file ~src:file ~dest:temp_file;
  temp_file

let merge_input_files files =
  let filename =
    if !Flag.use_temp then
      !!make_temp_file
    else
      Filename.change_extension (List.hd files) "merge.ml"
  in
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
  if not !Flag.Mode.ignore_conf then read_option_conf ()

let open_input_file () =
  let filename =
    match !Flag.filenames with
    | []
    | ["-"] ->
        let filename = if !Flag.use_temp then !!make_temp_file else "stdin.ml" in
        Flag.filenames := [filename];
        IO.output_file filename (IO.input_all stdin);
        filename
    | _ ->
        let filename =
          match !Flag.filenames with
          | [] -> assert false
          | [file] -> if !Flag.use_temp then copy_input_file file else file
          | files -> merge_input_files files
        in
        Config.load_path := List.map Filename.dirname !Flag.filenames @ !Config.load_path;
        filename
  in
  Flag.Method.input_cegar := List.length !Flag.filenames = 1 && String.ends_with (List.hd !Flag.filenames) ".cegar";
  Flag.mainfile := filename;
  open_in filename

(* called before parsing options *)
let fpat_init1 () =
  Fpat.FPATConfig.set_default ()

(* called after parsing options *)
let fpat_init2 () =
  let open Fpat in
  Global.target_filename := !Flag.mainfile;
  SMTProver.cvc3_command := !Flag.cvc3;
  SMTProver.initialize ()

let check_env () =
  begin
    match !Flag.ModelCheck.mc with
    | Flag.ModelCheck.TRecS -> if not Mconfig.trecs_available then fatal "TRecS not found"
    | Flag.ModelCheck.HorSat -> if not Mconfig.horsat_available then fatal "HorSat not found"
    | Flag.ModelCheck.HorSat2 -> if not Mconfig.horsat2_available then fatal "HorSat2 not found"
    | Flag.ModelCheck.HorSatP -> if not Mconfig.horsatp_available then fatal "HorSatP not found"
  end;
  begin
    if !Flag.Refine.use_rec_chc_solver then
      match !Flag.Refine.solver with
      | Flag.Refine.Hoice -> if not Mconfig.hoice_available then fatal "HoICE not found"
      | Flag.Refine.Z3
      | Flag.Refine.Z3_spacer -> if not Mconfig.z3_available then fatal "Z3 binary not found"
  end;
  begin
    if Flag.Experiment.HORS_quickcheck.(!use <> None) then
      if not Mconfig.hors_quickcheck_available then fatal "hors_quickcheck not found"
  end

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
  | Fpat.Timer.Timeout
  | Assert_failure("timer.ml", _, _) -> "TimeOut"
  | Killed -> "Killed"
  | e -> Printexc.to_string e

let print_error = function
  | Fpat.RefTypInfer.FailedToRefineTypes ->
      Format.eprintf "Verification failed:@.";
      Format.eprintf "  MoCHi could not refute an infeasible error path@.";
      Format.eprintf "  due to the incompleteness of the refinement type system@."
  | e when Fpat.FPATConfig.is_fpat_exception e ->
      Format.eprintf "FPAT: %a@." Fpat.FPATConfig.pr_exception e
  | Syntaxerr.Error err ->
      Format.eprintf "%a@." Syntaxerr.report_error err
  | Typecore.Error(loc,env,err) ->
      Format.eprintf "%a%a@." Location.print_error loc (Typecore.report_error env) err
  | Typemod.Error(loc,env,err) ->
      Format.eprintf "%a%a@." Location.print_error loc (Typemod.report_error env) err
  | Env.Error e ->
      Format.eprintf "%a@." Env.report_error e
  | Typetexp.Error(loc,env,err) ->
      Format.eprintf "%a%a@." Location.print_error loc (Typetexp.report_error env) err
  | Lexer.Error(err, loc) ->
      Format.eprintf "%a%a@." Location.print_error loc Lexer.report_error err
  | CEGAR_syntax.NoProgress ->
      Format.eprintf "Unknown. (CEGAR not progress) @."
  | CEGAR_abst.NotRefined ->
      Format.eprintf "Verification failed (new error path not found)@."
  | Fatal s ->
      Format.eprintf "Fatal error: %s@." s
  | Unsupported s ->
      Format.eprintf "Unsupported: %s@." s
  | Sys_error s ->
      Format.eprintf "%s@." s
  | TimeOut
  | Fpat.Timer.Timeout
  | Assert_failure("timer.ml", _, _) ->
      Format.eprintf "Verification failed (time out)@."
  | e when !Flag.Debug.debug_module = [] ->
      Format.eprintf "Exception: %s@." @@ Printexc.to_string e
  | e -> raise e

let init_before_parse_arg () =
  fpat_init1 ()

let init_after_parse_arg () =
  if Flag.ModelCheck.(!mc <> TRecS) then
    Flag.ModelCheck.church_encode := true;
  fpat_init2 ();
  ignore @@ Unix.alarm !Flag.time_limit;
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise TimeOut));
  Color.init ();
  check_env ()

let timeout_handler _ = raise TimeOut

let () =
  if !Sys.interactive
  then ()
  else
    try
      init_before_parse_arg ();
      parse_arg ();
      if not !!is_only_result then print_env true false;
      let cin = open_input_file () in
      init_after_parse_arg ();
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
        print_error e;
        exit 1
