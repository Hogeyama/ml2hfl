exception TimeOut
exception LongInput
exception CannotDiscoverPredicate


let print_info () =
  Format.printf "cycles: %d\n" !Flag.cegar_loop;
  Format.printf "total: %.3f sec\n" (Util.get_time());
  Format.printf "  abst: %.3f sec\n" !Flag.time_abstraction;
  Format.printf "  mc: %.3f sec\n" !Flag.time_mc;
  Format.printf "  refine: %.3f sec\n" !Flag.time_cegar;
  if false && Flag.debug then Format.printf "IP: %.3f sec\n" !Flag.time_interpolant;
  Format.printf "    exparam: %.3f sec\n" !Flag.time_parameter_inference;
  Format.pp_print_flush Format.std_formatter ()


let print_env () =
  let commit =
    try
      let cin = open_in "COMMIT" in
      let s = input_line cin in
      close_in cin;
      s
    with Sys_error _ | End_of_file -> ""
  in
  let trecs_version = TrecsInterface.version () in
  Format.printf "MoCHi: Model Checker for Higher-Order Programs@.";
  if commit <> "" then Format.printf "  Build: %s@." commit;
  if trecs_version <> "" then Format.printf "  TRecS version: %s@." trecs_version;
  Format.printf "  OCaml version: %s@." Sys.ocaml_version;
  Format.printf "  Command: %a@." (Util.print_list Format.pp_print_string " " false) (Array.to_list Sys.argv);
  Format.printf "@."; ()


(***** termination *****)
let threshold = ref 5

exception FailedToFindLLRF

let rec termination_loop predicate_que holed =
  let _ =
    begin
      threshold := !threshold - 1;
      if !threshold < 0 then (raise FailedToFindLLRF)
    end
  in
  if Queue.is_empty predicate_que then (raise FailedToFindLLRF)
  else
    let predicate_info = Queue.pop predicate_que in
    let predicate = BRA_transform.construct_LLRF predicate_info in
    let transformed = BRA_transform.pluging holed predicate in
    let orig, transformed = BRA_transform.retyping transformed in
    try
      Main_loop.run orig transformed
    with Refine.PostCondition (_, spc) ->
      let open Fpat in
      let unwrap_template (Term.App ([], Term.App ([], _, t), _)) = t in
      let imply t1 t2 = Formula.band [t1; Formula.bnot t2] in

      let arg_vars =
	List.map (fun v -> Var.of_string (Id.name (BRA_transform.extract_id v)))
	  (BRA_state.get_argvars holed.BRA_types.state holed.BRA_types.verified) in
      let arg_var_terms = List.map Term.make_var arg_vars in
      let prev_vars =
	List.map (fun v -> Var.of_string (Id.name (BRA_transform.extract_id v)))
	  (BRA_state.get_prev_statevars holed.BRA_types.state holed.BRA_types.verified) in
      let prev_var_terms = List.map Term.make_var prev_vars in
      let arg_env = List.map (fun a -> (a, SimType.int_type)) arg_vars in
      let prev_env = List.map (fun a -> (a, SimType.int_type)) prev_vars in

      let linear_template = unwrap_template (NonLinConstr.gen_template arg_env) in
      let linear_template_prev = Term.subst (List.combine arg_vars prev_var_terms) linear_template in

      let ranking_constraints =
	Formula.band [ Formula.gt linear_template_prev linear_template
		     ; Formula.geq linear_template (IntTerm.make 0)]
      in
      let constraints = imply spc ranking_constraints in
               (************ BUG!: obtain no coeffs ************)
      let coeff_constrs = NonLinConstr.gen_coeff_constrs constraints in
      let coefficients =
        try
          NonLinConstr.solve_constrs [] [] (Formula.band coeff_constrs)
        with NonLinConstr.Unknown ->
          assert false(* failed to solve the constraints *)
      in
      Format.printf "Linear template:@.  %a@." Term.pr linear_template;
      Format.printf "LLRF constraint:@.  %a@." Term.pr constraints;
      Format.printf "Constraint:@.  %a@." Term.pr_list coeff_constrs;
      Format.printf "Infered coefficients:@.  %a@." NonLinConstr.pr_coeffs coefficients;
      Format.printf "Unsafe!@.@.";
      false

let main in_channel =
  let input_string =
    let s = String.create Flag.max_input_size in
    let n = Util.my_input in_channel s 0 Flag.max_input_size in
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
    then Format.printf "parsed::@. @[%a@.@." Syntax.pp_print_term parsed
  in
  if !Flag.split_assert
  then
    let paths = Trans.search_fail parsed in
    let ts = List.map (fun path -> Trans.screen_fail path parsed) paths in
    List.for_all (Main_loop.run orig) (List.rev ts);
  else if !Flag.termination then
    let open BRA_util in
    let parsed = BRA_transform.regularization parsed in
    let functions = BRA_transform.extract_functions parsed in
    let holed_list = BRA_transform.to_holed_programs parsed functions in
    List.for_all (fun holed ->
      let init_predicate_info = { BRA_types.variables = []; BRA_types.prev_variables = []; BRA_types.coefficients = []} in
      let predicate_que = Queue.create () in
      let _ = Queue.add init_predicate_info predicate_que in
      termination_loop predicate_que holed) holed_list
  else
    Main_loop.run orig parsed



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
   "-debug", Arg.Set_int Flag.debug_level, "<n>  Set debug level";
   (* preprocessing *)
   "-na", Arg.Clear Flag.init_trans, " Disable encoding of recursive data structures";
   "-lift-fv", Arg.Set Flag.lift_fv_only, " Lift variables which occur in a body";
   "-cps-naive", Arg.Set Flag.cps_simpl, " Use naive CPS transformation";
   "-ins-param-funarg", Arg.Set Flag.insert_param_funarg, " Insert an extra param for functions with function arguments";
   (* verifier *)
   "-it", Arg.Unit (fun _ -> Flag.cegar := Flag.CEGAR_InteractionType), " Interaction type based verifier";
   "-spec", Arg.Set_string Flag.spec_file, "<filename>  use <filename> as a specification";
   "-use-spec", Arg.Set Flag.use_spec, " use XYZ.spec for verifying XYZ.ml if exists (This option is ignored if -spec is used)";
   (* CEGAR *)
   "-split-assert", Arg.Set Flag.split_assert, " Reduce to verification of multiple programs (each program has only one assertion)";
   "-dpa", Arg.Set Flag.disable_predicate_accumulation, " Disable predicate accumulation";
   (* relatively complete verification *)
   "-rc", Arg.Set Flag.relative_complete, " Enable relatively complete verification from the begining";
   "-disable-rc", Arg.Set Flag.disable_relatively_complete_verification, " Disable relatively complete verification";
   "-nex", Arg.Set_int Fpat.Global.number_of_extra_params,
          " Number of inserted extra parameters for each functional argument";
   "-tbit", Arg.Set_int Fpat.Global.bits_threshold,
          " Threshold on the number of bits used in the bit-vector modeling";
   "-cc", Arg.Set Fpat.Global.enable_coeff_const,
          " Disable constant terms of extra parameters";
   "-aec", Arg.Set Fpat.Global.accumulate_ext_constrs,
          " Accumulate constraints on the coefficients of extra parameters";
   "-dph", Arg.Set Fpat.Global.disable_parameter_inference_heuristics,
          " Disable heuristics of instantiation parameter inference";
   (* predicate abstraction *)
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
   "-eap", Arg.Set Fpat.Global.extract_atomic_predicates, " Extract atomic predicates";
   "-mp", Arg.Set Fpat.Global.use_multiple_paths, " Use multiple infeasible error paths for predicate discovery";
   (* Horn clause solver *)
   "-gi",
     Arg.Unit (fun _ ->
       Fpat.HcSolver.ext_solve := Fpat.GenHcSolver.solve;
       Fpat.GenInterpProver.ext_interpolate := Fpat.ApronInterface.convex_hull_interpolate false),
     " Generalize constraints of multiple function calls by interpolation";
   "-gchi",
     Arg.Unit (fun _ ->
       Fpat.HcSolver.ext_solve := Fpat.GenHcSolver.solve;
       Fpat.GenInterpProver.ext_interpolate := Fpat.ApronInterface.convex_hull_interpolate true),
     " Generalize constraints of multiple function calls by convex hull and interpolation";
   "-gtcs",
     Arg.Unit (fun _ ->
       Fpat.HcSolver.ext_solve := Fpat.GenHcSolver.solve;
       Fpat.GenInterpProver.ext_interpolate := Fpat.TemplateBasedGenInterpProver.interpolate),
     " Generalize constraints of multiple function calls by template-based constraint solving";
   "-gssi",
     Arg.Unit (fun _ ->
       Fpat.HcSolver.ext_solve := Fpat.GenHcSolver.solve;
       Fpat.GenInterpProver.ext_interpolate := Fpat.YintInterface.solution_space_based_interpolate;
       Fpat.InterpProver.ext_interpolate := Fpat.YintInterface.interpolate),
     " Generalize constraints of multiple function calls by solution space-based interpolation";
   "-yhorn",
     Arg.Unit (fun _ ->
       Fpat.HcSolver.ext_solve := Fpat.YhornInterface.solve),
     " Solve Horn clauses by using Yint";

   "-ieb", Arg.Set Fpat.Global.encode_boolean,
     " Enable integer encoding of booleans";
   (* interpolating prover *)
   "-csisat",
     Arg.Unit (fun _ ->
       Fpat.InterpProver.ext_interpolate := Fpat.CsisatInterface.interpolate),
     " Use CSIsat interpolating prover";
   "-gcsisat",
     Arg.Unit (fun _ ->
       Fpat.InterpProver.ext_interpolate := Fpat.CsisatInterface.interpolate ~generalize:true ),
     " Use CSIsat interpolating prover with an ad hoc generalization heuristics";
   "-yint",
     Arg.Unit (fun _ ->
       Fpat.InterpProver.ext_interpolate := Fpat.YintInterface.interpolate),
     " Use Yint interpolating prover";
   (* termination mode *)
   "-termination",
     Arg.Unit (fun _ ->
       Flag.termination := true),
     " Check termination";
  ]


let () =
  if !Sys.interactive
  then ()
  else
    try
      let set_file name =
        if !Flag.filename <> "" (* case of "./mochi.opt file1 file2" *)
        then (Arg.usage arg_spec usage; exit 1);
        Flag.filename := name
      in
      (* default interpolating prover *)
      Fpat.InterpProver.ext_interpolate := Fpat.CsisatInterface.interpolate;
      (* default Horn clause solver *)
      Fpat.HcSolver.ext_solve := Fpat.BwHcSolver.solve;
      Arg.parse arg_spec set_file usage;
      Fpat.Global.print_log := !Flag.debug_level <> 0;
      Fpat.Global.cvc3 := !Flag.cvc3;
      let cin =
        match !Flag.filename with
            "" | "-" -> Flag.filename := "stdin"; stdin
          | _ -> open_in !Flag.filename
      in
        Fpat.Cvc3Interface.init ();
        Fpat.AtpInterface.init ();
        Fpat.Cvc3Interface.open_cvc3 ();
        Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise TimeOut));
        ignore (Unix.alarm Flag.time_limit);
        if not !Flag.only_result then print_env ();
        if main cin then decr Flag.cegar_loop;
        Fpat.Cvc3Interface.close_cvc3 ();
        print_info ()
    with
        Syntaxerr.Error err ->
          Format.printf "%a@." Syntaxerr.report_error err; exit 1
      | Typecore.Error(loc,err) ->
          Format.printf "%a%a@." Location.print_error loc Typecore.report_error err; exit 1
      | Typemod.Error(loc,err) ->
          Format.printf "%a%a@." Location.print_error loc Typemod.report_error err; exit 1
      | Env.Error e -> Format.printf "%a@." Env.report_error e; exit 1
      | Typetexp.Error(loc,err) ->
          Format.printf "%a%a@." Location.print_error loc Typetexp.report_error err; exit 1
      | Lexer.Error(err, loc) ->
          Format.printf "%a%a@." Location.print_error loc Lexer.report_error err; exit 1
      | LongInput -> Format.printf "Input is too long@."; exit 1
      | TimeOut -> Format.printf "@.Verification failed (time out)@."; exit 1
      | CEGAR.NoProgress -> Format.printf "Verification failed (new error path not found)@."; exit 1
      | Fpat.AbsTypeInfer.FailedToRefineTypes ->
          Format.printf "Verification failed:@.  MoCHi could not refute an infeasible error path @.  due to the incompleteness of the refinement type system@."; exit 1
      | Util.Fatal s ->
          Format.printf "Fatal error: %s@." s; exit 1
