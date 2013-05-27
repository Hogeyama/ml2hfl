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



let init () =
  Syntax.typ_excep := Type.TConstr("exn",true)

let preprocess t spec =
  let fun_list,t,get_rtyp =
    if !Flag.init_trans
    then
      let t' = Trans.make_ext_funs t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "make_ext_funs::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let t' = Trans.copy_poly_funs t in
      let fun_list = Syntax.get_top_funs t' in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "copy_poly::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let spec' = Spec.rename spec t in
      let () = Spec.print spec' in
      let t' = Trans.replace_typ spec'.Spec.abst_env t in
      let () =
        if true && !Flag.debug_level > 0 && spec <> Spec.init
        then Format.printf "add_preds::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let t' = Abstract.abstract_recdata t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "abst_recdata::@. @[%a@.@." Syntax.pp_print_term t' in
      let t = t' in
      let t',get_rtyp_list = Abstract.abstract_list t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "abst_list::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let get_rtyp = get_rtyp_list in
      let t' = Trans.inlined_f spec'.Spec.inlined_f t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "inlined::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      (*let t =
        if (match !Flag.refine with Flag.RefineRefType(_) -> true | _ -> false) && !Flag.relative_complete then
          let t = Trans.lift_fst_snd t in
          let t = FpatInterface.insert_extra_param t in (* THERE IS A BUG *)
            if true && !Flag.debug_level > 0 then Format.printf "insert_extra_param (%d added)::@. @[%a@.@.%a@.@."
              (List.length !FpatInterface.params) Syntax.pp_print_term t Syntax.pp_print_term' t;
            t
        else
          t
      in*)
      let t',get_rtyp_cps_trans = CPS.trans t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "CPS::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let get_rtyp f typ = get_rtyp f (get_rtyp_cps_trans f typ) in
      let t',get_rtyp_remove_pair = CPS.remove_pair t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "remove_pair::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let get_rtyp f typ = get_rtyp f (get_rtyp_remove_pair f typ) in
      let t = t' in
      let t' = if !Flag.insert_param_funarg then Trans.insert_param_funarg t else t in
      let () =
        if !Flag.debug_level > 0 && t <> t'
        then Format.printf "insert unit param::@. @[%a@.@." Syntax.pp_print_term t'
      in
      fun_list, t', get_rtyp
    else Syntax.get_top_funs t, t, fun _ typ -> typ
  in

  let () = Type_check.check t Type.TUnit in
  let prog,map,rmap,get_rtyp_trans = CEGAR_util.trans_prog t in
  let get_rtyp f typ = get_rtyp f (get_rtyp_trans f typ) in

  let info =
    let fun_list =
      let aux x =
        try [List.assoc (CEGAR_util.trans_var x) map] with Not_found -> []
      in
        Util.rev_flatten_map aux fun_list
    in
    let inlined = List.map CEGAR_util.trans_var spec.Spec.inlined in
      {CEGAR.orig_fun_list=fun_list; CEGAR.inlined=inlined}
  in
    prog, rmap, get_rtyp, info

let rec main_loop orig parsed =
  let () = init () in
  let t = parsed in
  let () =
    if false && !Flag.debug_level > 0
    then Format.printf "parsed::@. @[%a@.@." Syntax.pp_print_term' t
  in
  let () =
    if !Flag.use_spec && !Flag.spec_file = ""
    then
      try
        let spec = Filename.chop_extension !Flag.filename ^ ".spec" in
        if Sys.file_exists spec then Flag.spec_file := spec
      with Invalid_argument "Filename.chop_extension" -> ()
  in
  let spec = Spec.parse Spec_parser.spec Spec_lexer.token !Flag.spec_file in
  let () = Spec.print spec in
  let main_fun,arg_num,t = if !Flag.cegar = Flag.CEGAR_DependentType then Trans.set_target t else "",0,t in
  let set_target = t in
  let () =
    if true && !Flag.debug_level > 0
    then Format.printf "set_target::@. @[%a@.@." Syntax.pp_print_term t
  in
  (** Unno: I temporally placed the following code here
            so that we can infer refinement types for a safe program
            with extra parameters added *)
  let t0 =
    if (match !Flag.refine with Flag.RefineRefType(_) -> true | _ -> false) && !Flag.relative_complete then
      let t = Trans.lift_fst_snd t in
      let t = FpatInterface.insert_extra_param t in (* THERE IS A BUG *)
        if true && !Flag.debug_level > 0 then Format.printf "insert_extra_param (%d added)::@. @[%a@.@.%a@.@."
          (List.length !FpatInterface.params) Syntax.pp_print_term t Syntax.pp_print_term' t;
        t
    else
      t
  in
  (**)
  let prog, rmap, get_rtyp, info = preprocess t0 spec in
    match !Flag.cegar with
        Flag.CEGAR_InteractionType ->
          FpatInterface.verify [] prog;
          assert false;
      | Flag.CEGAR_DependentType ->
          try
            match CEGAR.cegar prog info with
                prog', CEGAR.Safe env ->
                  if Flag.print_ref_typ
                  then
                    begin
                      Format.printf "Refinement types:@.";
                      List.iter (fun (f,typ) -> Format.printf "  %s: %a@." f CEGAR_ref_type.print typ) env;
                      Format.printf "@."
                    end;
                  let env' =
                    let aux (f,rtyp) : (Syntax.id * Ref_type.t) list =
                      try
                        let f' = List.assoc f rmap in
                          [f', Ref_type.rename (get_rtyp f' rtyp)]
                      with
                          Not_found -> []
                        | _ -> Format.printf "unimplemented or bug@.@."; []
                    in
                    if !Flag.insert_param_funarg
                    then []
                    else
                      if !Flag.relative_complete then
                        let _ = Flag.web := true in
                        let res = Util.rev_map_flatten aux env in
                        let _ = Flag.web := false in
                        res
                      else
                        Util.rev_map_flatten aux env
                  in
                  let () =
                    if !Flag.write_annot
                    then
                      let env'' = List.map (fun (id, typ) -> Id.name id, typ) env' in
                        WriteAnnot.f !Flag.filename orig env''
                  in
                  Format.printf "Safe!@.@.";
                  let () =
                    if !Flag.relative_complete then begin
                      let map =
                        List.map
                          (fun (x, n) ->
                            Id.make (-1) (Fpat.Var.string_of x) Type.TInt,
                            CEGAR_util.trans_inv_term
                              (FpatInterface.inv_term
                                (Fpat.IntTerm.make n)))
                          !Fpat.ParamSubstInfer.ext_coeffs
                      in
                      let t = Syntax.subst_map map t0 in
                      Format.printf "Program with Quantifiers Added:@.";
                      Flag.web := true;
                      Format.printf "  @[<v>%a@]@.@." Syntax.pp_print_term t;
                      Flag.web := false
                    end
                  in
                  if env' <> [] then Format.printf "Refinement Types:@.";
                  let env' = List.map (fun (f, typ) -> f, FpatInterface.simplify typ) env' in
                  let pr (f,typ) =
                    Format.printf "  %s: %a@." (Id.name f) Ref_type.print typ
                  in
                  List.iter pr env';
                  if env' <> [] then Format.printf "@.";
                  true
              | _, CEGAR.Unsafe ce ->
                Format.printf "Unsafe!@.@.";
                if main_fun <> "" && arg_num <> 0
                then
                  Format.printf "Input for %s:@.  %a@." main_fun
                    (Util.print_list Format.pp_print_int "; " false) (Util.take ce arg_num);
                Format.printf "@[<v 2>Error trace:%a@."  Eval.print (ce,set_target);
                false
          with
              Fpat.AbsTypeInfer.FailedToRefineTypes when not !Flag.insert_param_funarg ->
                Flag.insert_param_funarg := true;
                main_loop orig parsed
            | Fpat.AbsTypeInfer.FailedToRefineTypes when not !Flag.relative_complete && not !Flag.disable_relatively_complete_verification ->
                Format.printf "@.REFINEMENT FAILED!@.";
                Format.printf "Restart with relative_complete := true@.@.";
                Flag.relative_complete := true;
                main_loop orig parsed
            | Fpat.AbsTypeInfer.FailedToRefineTypes ->
                raise Fpat.AbsTypeInfer.FailedToRefineTypes
            | Fpat.ParamSubstInfer.FailedToRefineExtraParameters ->
                FpatInterface.params := [];
                Fpat.ParamSubstInfer.ext_coeffs := [];
                Fpat.ParamSubstInfer.ext_constrs := [];
                incr Fpat.Global.number_of_extra_params;
                main_loop orig parsed


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
      main_loop orig transformed
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
      let arg_env = List.map (fun a -> (a, SimType.Int)) arg_vars in
      let prev_env = List.map (fun a -> (a, SimType.Int)) prev_vars in

      let linear_template = unwrap_template (NonLinConstr.gen_template arg_env) in
      let linear_template_prev = Term.subst (List.combine arg_vars prev_var_terms) linear_template in

      let ranking_constraints =
	Formula.band [ Formula.gt linear_template_prev linear_template
		     ; Formula.geq linear_template (IntTerm.make 0)]
      in
      let constraints = imply spc ranking_constraints in
      let coeff_constrs = NonLinConstr.gen_coeff_constrs constraints in
      let coefficients =
        try
          NonLinConstr.solve_constrs [] [] (Formula.band coeff_constrs)
        with NonLinConstr.Unknown ->
	  Format.printf "Failed to solve the constraints...@.@.";
          assert false(* failed to solve the constraints *)
      in
      let ((correspondence_, const_part) as ranking_function) = ParLinExp.parlinexp_of (Term.subst (List.map (fun (v, c) -> (v, Term.make_const (Const.Int c))) coefficients) linear_template)
      in
      let correspondence =
	let cor dict x =
	  try List.assoc x dict with Not_found -> 0 in
	let formatter (n, v) = (v, IntTerm.int_of n) in
	List.map (cor (List.map formatter correspondence_)) arg_vars
      in
      let new_predicate_info =
	BRA_types.updated_predicate_info
	  predicate_info
	  (correspondence @ [IntTerm.int_of const_part])
      in
      Format.printf "Linear template:@.  %a@." Term.pr linear_template;
      Format.printf "LLRF constraint:@.  %a@." Term.pr constraints;
      Format.printf "Constraint:@.  %a@." Term.pr_list coeff_constrs;
      Format.printf "Inferred coefficients:@.  %a@." NonLinConstr.pr_coeffs coefficients;
      Format.printf "Ranking function:@.  %a@." ParLinExp.pr ranking_function;
      let _ = Queue.push new_predicate_info predicate_que in
      termination_loop predicate_que holed
	
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
  let parsed = Parser_wrapper.from_use_file orig in
  let () =
    if true && !Flag.debug_level > 0
    then Format.printf "parsed::@. @[%a@.@." Syntax.pp_print_term parsed
  in
  if !Flag.split_assert
  then
    let paths = Trans.search_fail parsed in
    let ts = List.map (fun path -> Trans.screen_fail path parsed) paths in
    List.for_all (main_loop orig) (List.rev ts);
  else if !Flag.termination then 
    let open BRA_util in
    let parsed = BRA_transform.regularization parsed in
    let functions = BRA_transform.extract_functions parsed in
    let holed_list = BRA_transform.to_holed_programs parsed functions in
    List.for_all (fun holed ->
      CEGAR.false_embedded := None;
      let init_predicate_info =
	{ BRA_types.variables = List.map BRA_transform.extract_id (BRA_state.get_argvars holed.BRA_types.state holed.BRA_types.verified)
	; BRA_types.prev_variables = List.map BRA_transform.extract_id (BRA_state.get_prev_statevars holed.BRA_types.state holed.BRA_types.verified)
	; BRA_types.coefficients = []} in
      let predicate_que = Queue.create () in
      let _ = Queue.add init_predicate_info predicate_que in
      termination_loop predicate_que holed) holed_list
  else
    main_loop orig parsed



let usage =  "Usage: " ^ Sys.executable_name ^ " [options] file\noptions are:"
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
   "-rd", Arg.Unit (fun _ -> Flag.refine := Flag.RefineRefTypeOld),
          " Use refinement type based predicate discovery (obsolete)";
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
        Wrapper.open_cvc3 ();
        Wrapper2.open_cvc3 ();
        Fpat.Cvc3Interface.init ();
        Fpat.AtpInterface.init ();
        Fpat.Cvc3Interface.open_cvc3 ();
        Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise TimeOut));
        ignore (Unix.alarm Flag.time_limit);
        if main cin then decr Flag.cegar_loop;
        Fpat.Cvc3Interface.close_cvc3 ();
        Wrapper2.close_cvc3 ();
        Wrapper.close_cvc3 ();
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
