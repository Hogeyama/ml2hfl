exception TimeOut
exception LongInput
exception CannotDiscoverPredicate

<<<<<<< HEAD
(***** information *****)
let lrf = ref []
let verified_function = ref ""
=======
>>>>>>> master

let print_info () =
  if !Flag.termination then
    begin
      List.iter
	(fun (f_name, pred) ->
	  Format.printf "ranking function(%s): %a\n" f_name BRA_types.pr_ranking_function pred)
	!lrf
    end;
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
<<<<<<< HEAD
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
                  (if !Flag.termination then
		      Format.printf "Terminating!(%s)@.@." !verified_function
		   else
                      Format.printf "Safe!@.@.");
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
                (if !Flag.termination then
		    Format.printf "Possibly non-terminating!(%s)@.@." !verified_function
		 else
                    Format.printf "UnSafe!@.@.");
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
=======
  let trecs_version = TrecsInterface.version () in
  Format.printf "MoCHi: Model Checker for Higher-Order Programs@.";
  if commit <> "" then Format.printf "  Build: %s@." commit;
  if trecs_version <> "" then Format.printf "  TRecS version: %s@." trecs_version;
  Format.printf "  OCaml version: %s@." Sys.ocaml_version;
  Format.printf "  Command: %a@." (Util.print_list Format.pp_print_string " " false) (Array.to_list Sys.argv);
  Format.printf "@."; ()
>>>>>>> master


(***** termination *****)
let threshold = ref 5

exception FailedToFindLLRF

let rec termination_loop predicate_que holed =
  let debug = true (*!Flag.debug_level > 0 *)in
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
    lrf := BRA_util.update_assoc (holed.BRA_types.verified.BRA_types.id.Id.name, predicate_info) !lrf; (* result log update here *)
    let transformed = BRA_transform.pluging holed predicate in
    let orig, transformed = BRA_transform.retyping transformed in
    try
<<<<<<< HEAD
      main_loop orig transformed
    with Refine.PostCondition (env, spc) ->
      let open Fpat in
      let unwrap_template (Term.App ([], Term.App ([], _, t), _)) = t in
      let imply t1 t2 = Formula.band [t1; Formula.bnot t2] in 
=======
      Main_loop.run orig transformed
    with Refine.PostCondition (_, spc) ->
      let open Fpat in
      let unwrap_template (Term.App ([], Term.App ([], _, t), _)) = t in
      let imply t1 t2 = Formula.band [t1; Formula.bnot t2] in

>>>>>>> master
      let arg_vars =
	List.map (fun v -> Var.of_string (Id.to_string (BRA_transform.extract_id v)))
	  (BRA_state.get_argvars holed.BRA_types.state holed.BRA_types.verified) in
      let arg_var_terms = List.map Term.make_var arg_vars in
      let prev_vars =
	List.map (fun v -> Var.of_string (Id.to_string (BRA_transform.extract_id v)))
	  (BRA_state.get_prev_statevars holed.BRA_types.state holed.BRA_types.verified) in
      let prev_var_terms = List.map Term.make_var prev_vars in
      let arg_env = List.map (fun a -> (a, SimType.Int)) arg_vars in
      let prev_env = List.map (fun a -> (a, SimType.Int)) prev_vars in
      
      if !Flag.disjunctive then
	(* make templates *)
	let linear_template = unwrap_template (NonLinConstr.gen_template arg_env) in
	let linear_template_prev = Term.subst (List.combine arg_vars prev_var_terms) linear_template in
	if debug then Format.printf "Linear template:@.  %a@." Term.pr linear_template;

	(* spc => R(x_prev) > R(x) && R(x) >= 0 *)
	let ranking_constraints =
	  Formula.band [ Formula.gt linear_template_prev linear_template
		       ; Formula.geq linear_template (IntTerm.make 0)]
	in
	let constraints = imply spc ranking_constraints in
	let coeff_constrs = NonLinConstr.gen_coeff_constrs constraints in
	if debug then Format.printf "Constraint:@.  %a@." Term.pr_list coeff_constrs;
	
        (* solve constraints and obtain coefficients of a ranking function *)
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

<<<<<<< HEAD
        (* update predicate *)
	let new_predicate_info =
	  BRA_types.updated_predicate_info
	    predicate_info
	    ({BRA_types.coeffs = correspondence; BRA_types.constant = IntTerm.int_of const_part} :: predicate_info.BRA_types.coefficients)
	    (spc :: predicate_info.BRA_types.error_paths)
	in
	if debug then Format.printf "\nInferred coefficients:@.  %a@." NonLinConstr.pr_coeffs coefficients;
	if debug then Format.printf "Ranking function:@.  %a@." ParLinExp.pr ranking_function;
	let _ = Queue.push new_predicate_info predicate_que in
	termination_loop predicate_que holed
      else
	(* all order constructed by <spc_1, spc_2, ..., spc_n> and newly obtained spc *)
	let spc_sequences =
	  let rec go l = function
	    | [] -> [l@[spc]]
	    | x::xs as r -> (l@[spc]@r) :: go (l@[x]) xs
	  in go [] predicate_info.BRA_types.error_paths
	in
	
	let successes = BRA_util.concat_map (fun error_paths ->
	  (* make templates *)
	  (** R(x)とR(x_prev)を作成 → いまは1つだけだが、これをn個用意する必要がある。linear_templates = {R1(x) .. Rn(x)}, linear_templates = {R1(x_prev) .. Rn(x_prev)} **)
	  let linear_templates = List.mapi (fun i _ -> unwrap_template (NonLinConstr.gen_template arg_env)) error_paths in
	  let linear_templates_prev = List.mapi (fun i lt -> Term.subst (List.combine arg_vars prev_var_terms) lt) linear_templates in
	  if debug then List.iteri (fun i lt -> Format.printf "Linear template(%d):@.  %a@." i Term.pr lt) linear_templates;
	  (** 作るべき制約は、
	      [φ1 ⇒ R1(x_prev)＞R1(x) ∧ R1(x)≧0]
	      ∧ [φ2 ⇒ R1(x_prev)＝R1(x) ∧ R2(x_prev)＞R2(x) ∧ R2(x)≧0]
	      ∧ [φ3 ⇒ R1(x_prev)＝R1(x) ∧ R2(x_prev)＝R2(x) ∧ R3(x_prev)＞R3(x) ∧ R3(x)≧0]
	      ...
	  **)
	  let all_vars = List.map fst env in
	  let subst_ith i = Term.subst (List.combine all_vars (List.map (fun v -> Term.make_var (Var.rename_base (fun (Idnt.Id v) -> Idnt.Id (v ^ "_IN_" ^ string_of_int i ^ "TH_ERRORPATH")) v)) all_vars)) in
	  let nth_constraints n nth_error_path =
	    let rec go i = 
	      let ith_ltp, ith_lt = List.nth linear_templates_prev i, List.nth linear_templates i in
	      if i < n then
		Formula.eqInt ith_ltp ith_lt :: go (i+1)
	      else
		[Formula.gt ith_ltp ith_lt; Formula.geq ith_lt (IntTerm.make 0)]
	    in
	    subst_ith n (imply nth_error_path (Formula.band (go 0)))
	  in
	  let constraints = Formula.bor (List.mapi nth_constraints error_paths)in
	  let coeff_constrs = NonLinConstr.gen_coeff_constrs constraints in
	  if debug then Format.printf "Constraint:@.  %a@." Term.pr constraints;
	  if debug then Format.printf "Constraints(Transformed by NonLinConstr.gen_coeff_constrs):@.  %a@." Term.pr_list coeff_constrs;
	  
	  try
	    (** 制約を解いて各テンプレートに代入。ここで手に入るのは全てのテンプレートに対する全ての引数の置換を含む代入である。 **)
	    let coefficients = NonLinConstr.solve_constrs [] [] (Formula.band coeff_constrs) in
	    if coefficients = [] then (Format.printf "@.Invalid ordered.@."; raise NonLinConstr.Unknown);
	    if debug then Format.printf "@.Inferred coefficients:@.  %a@." NonLinConstr.pr_coeffs coefficients;

	    let coefficient_infos = List.mapi (fun i ith_lt ->
	      let ((correspondence_, const_part) as ranking_function) = ParLinExp.parlinexp_of (Term.subst (List.map (fun (v, c) -> (v, Term.make_const (Const.Int c))) coefficients) ith_lt)
	      in
	      (** 引数の変数との対応関係を考え、linear ranking functionを係数情報に変換 **)
	      let correspondence =
		let cor dict x =
		  try List.assoc x dict with Not_found -> 0 in
		let formatter (n, v) = (v, IntTerm.int_of n) in
		List.map (cor (List.map formatter correspondence_)) arg_vars
	      in
	      if debug then Format.printf "Ranking function:@.  %a@." ParLinExp.pr ranking_function;
	      {BRA_types.coeffs = correspondence; BRA_types.constant = IntTerm.int_of const_part}
	    ) linear_templates
	    in
	    (************* ここまでは「各テンプレートについての」correspondenceを求める。 ************)
	    (** 新しい述語情報を生成 **)
	    let new_predicate_info =
	      BRA_types.updated_predicate_info
		predicate_info
		coefficient_infos
		error_paths
	    in
	    Format.printf "@.Found ranking function: %a@." BRA_types.pr_ranking_function new_predicate_info;
	    [new_predicate_info]
	  with NonLinConstr.Unknown ->
	    Format.printf "Failed to solve the constraints...@.@.";
            [] (* failed to solve the constraints *)
	) spc_sequences	
	in
	let _ = List.iter (fun pred -> Queue.push pred predicate_que) (List.rev successes) in
	termination_loop predicate_que holed
	
	
=======
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

>>>>>>> master
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
      verified_function := holed.BRA_types.verified.BRA_types.id.Id.name;
      let init_predicate_info =
	{ BRA_types.variables = List.map BRA_transform.extract_id (BRA_state.get_argvars holed.BRA_types.state holed.BRA_types.verified)
	; BRA_types.prev_variables = List.map BRA_transform.extract_id (BRA_state.get_prev_statevars holed.BRA_types.state holed.BRA_types.verified)
	; BRA_types.coefficients = []
	; BRA_types.error_paths = [] } in
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
   "-termination-disj",
     Arg.Unit (fun _ ->
       Flag.termination := true;
       Flag.disjunctive := true),
     " Check termination by finding disjunctive well-founded relation";
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
