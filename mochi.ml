open Util

exception TimeOut
exception LongInput

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
      if !Flag.add_closure_exparam && !Flag.result = "terminating" then
	Format.printf "exparam inserted program:@. %a@." Syntax.print_term !ExtraParamInfer.origWithExparam;
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
    let s = input_line cin in
    close_in cin;
    s
  with Sys_error _ | End_of_file -> ""


let print_commit_hash () =
  Format.printf "%s@." @@ get_commit_hash ()

let print_env () =
  let commit = get_commit_hash () in
  let trecs_version = TrecsInterface.version () in
  Color.printf Color.Green "MoCHi: Model Checker for Higher-Order Programs@.";
  if commit <> "" then Format.printf "  Build: %s@." commit;
  if trecs_version <> "" then Format.printf "  TRecS version: %s@." @@ trecs_version;
  Format.printf "  OCaml version: %s@." Sys.ocaml_version;
  Format.printf "  Command: %a@." (print_list Format.pp_print_string " ") !Flag.args;
  Format.printf "@."; ()


let main in_channel =
  let input_string =
    let s = String.create Flag.max_input_size in
    let n = my_input in_channel s 0 Flag.max_input_size in
      if n = Flag.max_input_size then raise LongInput;
      String.sub s 0 n
  in

  let lb = Lexing.from_string input_string in
  let () = lb.Lexing.lex_curr_p <-
    {Lexing.pos_fname = Filename.basename !Flag.filename;
     Lexing.pos_lnum = 1;
     Lexing.pos_cnum = 0;
     Lexing.pos_bol = 0};
  in
  let orig = Parse.use_file lb in
  Id.set_counter (Ident.current_time () + 1);
  let parsed = Parser_wrapper.from_use_file orig in
  let () =
    if true && !Flag.debug_level > 0
    then Format.printf "%a:@. @[%a@.@." Color.s_red "parsed" Syntax.print_term parsed
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
    let _ = if !Flag.debug_level > 0 then Format.printf "lambda-lifted::@. @[%a@.@." Syntax.print_term parsed in
    let parsed = BRA_transform.regularization parsed in
    let _ = if !Flag.debug_level > 0 then Format.printf "regularized::@. @[%a@.@." Syntax.print_term parsed in
    let parsed = if !Flag.add_closure_depth then ExtraClsDepth.addExtraClsDepth parsed else parsed in
    let _ = if !Flag.debug_level > 0 && !Flag.add_closure_depth then Format.printf "closure depth inserted::@. @[%a@.@." Syntax.print_term parsed in
    let parsed = if !Flag.add_closure_exparam then ExtraParamInfer.addTemplate parsed else parsed in
    let _ = if !Flag.debug_level > 0 && !Flag.add_closure_exparam then Format.printf "closure exparam inserted::@. @[%a@.@." Syntax.print_term parsed in
    let holed_list = BRA_transform.to_holed_programs parsed in
    let result =
      try
	List.for_all (fun holed ->
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



let print_option_and_exit = ref (fun () -> ())

let usage =
  "MoCHi: Model Checker for Higher-Order Programs\n\n" ^
    "Usage: " ^ Sys.executable_name ^ " [options] file\noptions are:"
let arg_spec =
  ["-I", Arg.String (fun dir -> Config.load_path := dir::!Config.load_path),
         "<dir>  Add <dir> to the list of include directories";
   "-margin", Arg.Int Format.set_margin, "<n>  Set pretty printing margin";
   "-only-result",
     Arg.Unit (fun () ->
                 Flag.only_result := true;
                 Flag.debug_level := 0;
                 Flag.print_progress := false),
     " Show only result";
   "-verbose", Arg.Set Fpat.Global.verbose, "Verbose mode";
   "-debug", Arg.Set_int Flag.debug_level, "<n>  Set debug level";
   "-color", Arg.Set Flag.color, " Turn on syntax highlighting";
   "-color-always", Arg.Set Flag.color_always, " Turn on syntax highlighting even if stdout does not refer to a terminal";
   "-ignore-conf", Arg.Set Flag.ignore_conf, " Ignore option.conf";
   "-exp", Arg.Unit (fun () ->
                       Flag.only_result := true;
                       Flag.debug_level := 0;
                       Flag.print_progress := false;
                       Flag.exp := true),
     " For experiments";
   "-v", Arg.Unit (fun () -> print_commit_hash (); exit 0), " Print the version shortly";
   "-version", Arg.Unit (fun () -> print_env (); exit 0), " Print the version";
   "-limit", Arg.Set_int Flag.time_limit, " Set time limit";
   "-option-list", Arg.Unit (fun () -> !print_option_and_exit ()), " Print list of options (for completion)";
   (* preprocessing *)
   "-list-option", Arg.Set Flag.encode_list_opt, " Encode list using options not pairs";
   "-na", Arg.Clear Flag.init_trans, " Disable encoding of recursive data structures";
   "-lift-fv", Arg.Set Flag.lift_fv_only, " Lift variables which occur in a body";
   "-cps-naive", Arg.Set Flag.cps_simpl, " Use naive CPS transformation";
   "-ins-param-funarg", Arg.Set Flag.insert_param_funarg, " Insert an extra param for functions with function arguments";
   "-tupling", Arg.Unit (fun () -> Flag.tupling := not !Flag.tupling; Flag.elim_same_arg := true), "Toggle tupling";
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
   "-disable-rc", Arg.Set Flag.disable_relatively_complete_verification, " Disable relatively complete verification";
   "-nex", Arg.Set_int Fpat.Global.number_of_extra_params,
          " Number of inserted extra parameters for each functional argument";
   "-tbit", Arg.Set_int Fpat.BvPolyConstrSolver.bits_threshold,
          " Threshold on the number of bits used in the bit-vector modeling";
   "-cc", Arg.Set Fpat.Global.enable_coeff_const,
          " Disable constant terms of extra parameters";
   "-aec", Arg.Set Fpat.EHCCSSolver.accumulate_ext_constrs,
          " Accumulate constraints on the coefficients of extra parameters";
   "-dph", Arg.Set Fpat.PolyConstrSolver.disable_parameter_inference_heuristics,
          " Disable heuristics of instantiation parameter inference";
   (* predicate abstraction *)
   "-wp-max", Arg.Set_int Flag.wp_max_num, " Maximum number of widths of conjunctions used in predicate abstraction";
   "-abs-remove-false", Arg.Set Flag.remove_false, " Do not use unsatisfiable predicates in abstraction";
   "-no-enr", Arg.Clear Flag.expand_nonrec, " Do not expand non-recursive functions";
   "-enr", Arg.Set Flag.expand_nonrec, " Expand non-recursive functions";
   "-enr2", Arg.Unit (fun _ -> Flag.expand_nonrec := true; Flag.expand_nonrec_init := false),
            " Expand non-recursive functions except those in the original program";
   "-abs-filter", Arg.Set Flag.use_filter, " Turn on the abstraction-filter option";
   "-neg-pred-on", Arg.Set Flag.use_neg_pred, " Use negative predicates for abstraction";
   "-neg-pred-off", Arg.Unit (fun _ -> Flag.use_neg_pred := false; Flag.never_use_neg_pred := true),
                    " Never use negative predicates for abstraction";
   (* higher-order model checking *)
   "-trecs", Arg.Set_string Flag.trecs,
             Format.sprintf "<cmd>  Change trecs command to <cmd> (default: \"%s\")" !Flag.trecs;
   "-ea", Arg.Set Flag.print_eval_abst, " Print evaluation of abstacted program";
   (* predicate discovery *)
   "-bool-init-empty", Arg.Set Flag.bool_init_empty,
                       " Use an empty set as the initial sets of predicates for booleans";
   "-rs", Arg.Unit (fun _ -> Flag.refine := Flag.RefineRefType(0)),
          " Use refinement type based predicate discovery (same as -rsn 0)";
   "-rsn", Arg.Int (fun n -> Flag.refine := Flag.RefineRefType(n)),
          "<num>  Use refinement type based predicate discovery";
   "-eap", Arg.Set Fpat.AbsType.extract_atomic_predicates, " Extract atomic predicates";
   "-enable-cp", Arg.Set Fpat.RefTypInfer.exploit_cut_points, " Exploit cut-points";
   "-mp", Arg.Set Fpat.Global.use_multiple_paths, " Use multiple infeasible error paths for predicate discovery";
   (* HCCS solver *)
   "-rscomp",
     Arg.Unit (fun _ ->
       Fpat.HCCSSolver.link_solver Fpat.RsCompHCCSSolver.solve),
     " Use a complete HCCS solver based on relaxed stratification";

   "-gi",
     Arg.Unit (fun _ ->
       Fpat.HCCSSolver.link_solver Fpat.GenHCCSSolver.solve;
       Fpat.GenInterpProver.ext_interpolate := Fpat.CHGenInterpProver.interpolate false),
     " Generalize constraints of multiple function calls by interpolation";
   "-size",
     Arg.Unit (fun _ ->
       Fpat.HCCSSolver.link_solver Fpat.BeautifulHCCSSolver.size),
     " measuing solution size";

   "-oldbdag",
     Arg.Unit (fun _ ->
       Fpat.HCCSSolver.link_solver Fpat.BeautifulHCCSSolver.solve),
     " Use beautiful dag HCCS solver (with old sampling-based beautiful tree HCCS solver)";
   "-bdag",
     Arg.Unit (fun _ ->
       Fpat.HCCSSolver.link_solver (Fpat.BeautifulDagHCCSSolver.solve_wo_sampling Fpat.BeautifulTreeHCCSSolver.solve)),
     " Use beautiful dag HCCS solver (with beautiful tree HCCS solver)";
   "-bdagm",
     Arg.Unit (fun _ ->
       Fpat.HCCSSolver.link_solver (Fpat.BeautifulDagHCCSSolver.solve_wo_sampling Fpat.BwHCCSSolver.solve)),
     " Use beautiful dag HCCS solver (with backward tree HCCS solver)";
   "-unwind",
     Arg.Unit (fun _ ->
       Fpat.HCCSSolver.link_solver (Fpat.UnwindDagHCCSSolver.solve Fpat.BwHCCSSolver.solve)),
     " Use dag HCCS solver based on dag unwinding (with backward tree HCCS solver)";
   "-unwindp",
     Arg.Unit (fun _ ->
       Fpat.HCCSSolver.link_solver (Fpat.UnwindDagHCCSSolver.solve Fpat.BeautifulTreeHCCSSolver.solve)),
     " Use dag HCCS solver based on dag unwinding (with beautiful tree HCCS solver)";
   "-gchi",
     Arg.Unit (fun _ ->
       Fpat.HCCSSolver.link_solver Fpat.GenHCCSSolver.solve;
       Fpat.GenInterpProver.ext_interpolate := Fpat.CHGenInterpProver.interpolate true),
     " Generalize constraints of multiple function calls by convex hull and interpolation";
   "-gtcs",
     Arg.Unit (fun _ ->
       Fpat.HCCSSolver.link_solver Fpat.GenHCCSSolver.solve;
       Fpat.GenInterpProver.ext_interpolate := Fpat.TemplateBasedGenInterpProver.interpolate),
     " Generalize constraints of multiple function calls by template-based constraint solving";
(* add option to disable simplification interp_simplify := false*)

   "-ieb", Arg.Set Fpat.Global.encode_boolean,
     " Enable integer encoding of booleans";
   "-tasp", Arg.Set Fpat.EncBoolHCCSSolver.encode_true_as_pos,
     " Encode true and false respectively as positive and non-positive integers";
   (* interpolating prover *)
   "-csisat",
     Arg.Unit (fun _ ->
       Fpat.InterpProver.ext_interpolate := Fpat.InterpProver.interpolate_csisat),
     " Use CSIsat interpolating prover";
   "-gcsisat",
     Arg.Unit (fun _ ->
       Fpat.InterpProver.ext_interpolate := Fpat.InterpProver.interpolate_csisat_gen),
     " Use CSIsat interpolating prover with an ad hoc generalization heuristics";
   "-tcs",
     Arg.Unit (fun _ ->
       Fpat.InterpProver.ext_interpolate :=
         Fpat.TemplateBasedInterpProver.interpolate (Fpat.Qelim.simplify_eqint_full [])),
     " Use an interpolating prover based on template based constraint solving";
   (* SMT solver *)
   "-z3", Arg.Unit (fun _ ->
       Fpat.SMTProver.init_z3 ();
       Fpat.PolyConstrSolver.ext_solve := Fpat.PolyConstrSolver.solve_z3),
     " Use Z3 SMT solver";
   "-cvc3", Arg.Unit (fun _ ->
       Fpat.SMTProver.init_cvc3 ();
       Fpat.PolyConstrSolver.ext_solve := Fpat.PolyConstrSolver.solve_cvc3),
     " Use CVC3 SMT solver";
   (* template based inference *)
   "-z3-template",
     Arg.Unit (fun _ ->
       Fpat.Template.ext_generate :=
         Fpat.PolyConstrSolver.gen_coeff_constr ~nat:false;
       Fpat.Template.ext_solve := Fpat.PolyConstrSolver.solve_z3),
     " Use Z3 SMT solver for template based inference";
   "-z3-template-lin",
     Arg.Unit (fun _ ->
       Fpat.Template.ext_generate :=
         Fpat.PolyConstrSolver.gen_coeff_constr ~nat:false ~linear:true;
       Fpat.Template.ext_solve := Fpat.PolyConstrSolver.solve_z3),
     " Use Z3 SMT solver for template based inference (norec)";
   "-mip-template",
     Arg.Unit (fun _ ->
         Fpat.Template.ext_generate :=
           Fpat.PolyConstrSolver.gen_coeff_constr ~nat:true ~linear:true;
         Fpat.Template.ext_solve :=
           Fpat.PolyConstrSolver.solve_glpk),
     " Use a template based inference based on mixed integer linear programming (norec)";
   "-cqp-template",
     Arg.Int (fun n ->
       Fpat.Global.cqp_mode := n;
       if n < 2 then begin
         Fpat.Template.ext_generate :=
           Fpat.PolyConstrSolver.gen_coeff_constr ~nat:false ~linear:true;
         Fpat.Template.ext_solve :=
           Fpat.PolyConstrSolver.solve_gsl
       end else begin
         Fpat.Template.ext_generate :=
           Fpat.PolyConstrSolver.gen_coeff_constr ~nat:true ~linear:true;
         Fpat.Template.ext_solve :=
           Fpat.PolyConstrSolver.solve_gsl
       end),
     " Use a template based inference based on convex quadratic programming (norec)";
   (* relatively complete verification *)
   "-bv-exparm",
     Arg.Unit (fun _ ->
       Fpat.EHCCSSolver.ext_generate :=
         Fpat.PolyConstrSolver.gen_coeff_constr ~nat:true;
       Fpat.EHCCSSolver.ext_solve :=
         Fpat.BvPolyConstrSolver.solve),
     " Use a bit-vector-based extra parameter inference";
   "-z3-exparam",
     Arg.Unit (fun _ ->
       Fpat.EHCCSSolver.ext_generate :=
         Fpat.PolyConstrSolver.gen_coeff_constr ~nat:false;
       Fpat.EHCCSSolver.ext_solve := Fpat.PolyConstrSolver.solve_z3),
     " Use Z3 SMT solver for extra parameter inference";
   (* termination mode *)
   "-z3-rank",
     Arg.Unit (fun _ ->
       Fpat.RankFunInfer.ext_generate :=
         Fpat.PolyConstrSolver.gen_coeff_constr ~nat:false;
       Fpat.RankFunInfer.ext_solve := Fpat.PolyConstrSolver.solve_z3),
     " Use Z3 SMT solver for ranking function inference";
   "-z3-rank-lin",
     Arg.Unit (fun _ ->
       Fpat.RankFunInfer.ext_generate :=
         Fpat.PolyConstrSolver.gen_coeff_constr ~nat:false ~linear:true;
       Fpat.RankFunInfer.ext_solve := Fpat.PolyConstrSolver.solve_z3),
     " Use Z3 SMT solver for ranking function inference (no exparam)";
   "-cqp-rank-lin",
     Arg.Unit (fun _ ->
       Fpat.RankFunInfer.ext_generate :=
         Fpat.PolyConstrSolver.gen_coeff_constr ~nat:false ~linear:true;
       Fpat.RankFunInfer.ext_solve :=
         Fpat.PolyConstrSolver.solve_gsl),
     " Use convex quadratic programming based ranking function inference (no exparam)";
   "-bv-rank-lin",
     Arg.Unit (fun _ ->
       Fpat.RankFunInfer.ext_generate :=
         Fpat.PolyConstrSolver.gen_coeff_constr ~nat:true ~linear:true;
       Fpat.RankFunInfer.ext_solve :=
         Fpat.BvPolyConstrSolver.solve),
     " Use bit-vector-based ranking function inference (no exparam)";
   "-bv-rank",
     Arg.Unit (fun _ ->
       Fpat.RankFunInfer.ext_generate :=
         Fpat.PolyConstrSolver.gen_coeff_constr ~nat:true (*~linear:true*);
       Fpat.RankFunInfer.ext_solve :=
         Fpat.BvPolyConstrSolver.solve),
     " Use bit-vector-based ranking function inference";
   "-rbf", (* use this with Z3 otherwise... *)
     Arg.Set Fpat.RankFunInfer.rank_bounded_first,
     " Try to infer a ranking function with small coefficients";
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

let () = print_option_and_exit := fun () ->
  List.iter (fun (s,_,_) -> Format.printf "%s " s) arg_spec; exit 0

let string_of_exception = function
    e when FpatInterface.is_fpat_exception e -> FpatInterface.string_of_error e
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
  Arg.parse (Arg.align arg_spec) set_file usage;
  Flag.args := Array.to_list Sys.argv;
  if not !Flag.ignore_conf
  then
    begin
      try
        let cin = open_in "option.conf" in
        let s = input_line cin in
        close_in cin;
        let args = Str.split (Str.regexp "[ \t]+") s in
        Arg.current := 0;
        Arg.parse_argv (Array.of_list @@ Sys.argv.(0) :: args) (Arg.align arg_spec) set_file usage;
        Flag.args := !Flag.args @ args
      with
        Arg.Bad s
      | Arg.Help s -> Format.printf "%s@." s; exit 1
      | Sys_error _
      | End_of_file -> ()
    end;
  match !Flag.filename with
    "" | "-" -> Flag.filename := "stdin"; stdin
  | _ -> open_in !Flag.filename


(* called before parsing options *)
let fpat_init1 () =
  let open Fpat in

  (* default interpolating prover *)
  InterpProver.ext_interpolate := Fpat.InterpProver.interpolate_csisat;

  (* default Horn clause solver *)
  HCCSSolver.link_solver BwHCCSSolver.solve;

  (* default solver for parameter substitution inference *)
  Fpat.EHCCSSolver.ext_generate :=
    Fpat.PolyConstrSolver.gen_coeff_constr ~nat:false;
  Fpat.EHCCSSolver.ext_solve := Fpat.PolyConstrSolver.solve_z3;

  (* default solver for ranking function inference *)
  Fpat.RankFunInfer.ext_generate :=
    Fpat.PolyConstrSolver.gen_coeff_constr ~nat:false (*~linear:true*);
  Fpat.RankFunInfer.ext_solve := Fpat.PolyConstrSolver.solve_z3;

  (* default solver for template based inference *)
  Fpat.Template.ext_generate :=
    Fpat.PolyConstrSolver.gen_coeff_constr ~nat:false;
  Fpat.Template.ext_solve := Fpat.PolyConstrSolver.solve_z3;

  (* default polynomial constraint solver *)
  Fpat.PolyConstrSolver.ext_solve := Fpat.PolyConstrSolver.solve_z3;

  (* default SMT solver *)
  Fpat.SMTProver.init_z3 ()

(* called after parsing options *)
let fpat_init2 () =
  let open Fpat in
  Global.target_filename := !Flag.filename;
  Global.print_log := !Flag.debug_level > 1;
  Global.cvc3 := !Flag.cvc3;
  SMTProver.open_ ()

let () =
  if !Sys.interactive
  then ()
  else
    try
      Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise TimeOut));
      ignore (Unix.alarm !Flag.time_limit);
      fpat_init1 ();
      let cin = parse_arg () in
      fpat_init2 ();
      Color.init ();
      if not !Flag.only_result then print_env ();
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
      | Fpat.AbsTypInfer.FailedToRefineTypes ->
          Format.printf "Verification failed:@.";
          Format.printf "  MoCHi could not refute an infeasible error path @.";
          Format.printf "  due to the incompleteness of the refinement type system@."
      | e when FpatInterface.is_fpat_exception e ->
          Format.printf "FPAT: %a@." FpatInterface.report_error e
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
(*      | Fatal s ->
          Format.printf "Fatal error: %s@." s
 *)
      | Util.Unsupported s ->
          Format.printf "Unsupported: %s@." s
