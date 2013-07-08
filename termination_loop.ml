(***** information *****)

let lrf = ref []
let max_threshold = 5

exception FailedToFindLLRF

let counter = ref 0
let get_now () = (counter := !counter + 1; !counter - 1)
let reset_counter () = counter := 0

let cycle_counter = ref 0
let incr_cycle () = cycle_counter := !cycle_counter + 1
let reset_cycle () = cycle_counter := 0

let verify_with holed pred =
  let debug = !Flag.debug_level > 0 in
  (* combine holed program and predicate *)
  let transformed = BRA_transform.pluging holed pred in
  if debug then Format.printf "[%d]@.%a@." (get_now ()) Syntax.pp_print_term transformed;
  let orig, transformed = BRA_transform.retyping transformed (BRA_state.type_of_state holed) in
  Main_loop.run orig transformed

let rec run predicate_que holed =
  let debug = !Flag.debug_level > 0 in
  let _ =
    begin
      incr_cycle ();
      if !cycle_counter > max_threshold then (raise FailedToFindLLRF)
    end
  in
  if Queue.is_empty predicate_que then (raise FailedToFindLLRF)
  else
    let predicate_info = Queue.pop predicate_que in
    lrf := BRA_util.update_assoc (holed.BRA_types.verified.BRA_types.id.Id.name, !cycle_counter, predicate_info) !lrf; (* result log update here *)
    try
      let result = if !Flag.separate_pred then
	  let predicates = BRA_transform.separate_to_CNF (BRA_transform.construct_LLRF predicate_info) in
          List.for_all (verify_with holed) predicates
	else if !Flag.split_callsite then
	  let predicate = BRA_transform.construct_LLRF predicate_info in
	  let splited = BRA_transform.callsite_split holed in
	  reset_counter ();
	  List.for_all (fun h -> verify_with h predicate) splited
	else
	  let predicate = BRA_transform.construct_LLRF predicate_info in
	  verify_with holed predicate
      in
      if result then
	(if not !Flag.exp then Format.printf "%s is terminating.@." holed.BRA_types.verified.BRA_types.id.Id.name ; result)
      else
	(if not !Flag.exp then Format.printf "%s is possibly non-terminating.@." holed.BRA_types.verified.BRA_types.id.Id.name ; result)
    with Refine.PostCondition (env, spc) ->
      let open Fpat in
      let unwrap_template (Term.App ([], Term.App ([], _, t), _)) = t in
      let imply t1 t2 = Formula.band [t1; Formula.bnot t2] in
      let arg_vars =
	List.map (fun v -> Var.of_string (Id.to_string (BRA_transform.extract_id v)))
	  (BRA_state.get_argvars holed.BRA_types.state holed.BRA_types.verified) in
      let arg_var_terms = List.map Term.make_var arg_vars in
      let prev_vars =
	List.map (fun v -> Var.of_string (Id.to_string (BRA_transform.extract_id v)))
	  (BRA_state.get_prev_statevars holed.BRA_types.state holed.BRA_types.verified) in
      let prev_var_terms = List.map Term.make_var prev_vars in
      let arg_env = List.map (fun a -> (a, SimType.int_type)) arg_vars in
      let prev_env = List.map (fun a -> (a, SimType.int_type)) prev_vars in
      
      if !Flag.disjunctive then
	(* make templates *)
	let linear_template = unwrap_template (PolyConstrSolver.gen_template arg_env) in
	let linear_template_prev = Term.subst (List.combine arg_vars prev_var_terms) linear_template in
	if debug then Format.printf "Linear template:@.  %a@." Term.pr linear_template;

	(* spc => R(x_prev) > R(x) && R(x) >= 0 *)
	let ranking_constraints =
	  Formula.band [ Formula.gt linear_template_prev linear_template
		       ; Formula.geq linear_template (IntTerm.make 0)]
	in
	let constraints = imply spc ranking_constraints in
	let coeff_constrs = PolyConstrSolver.generate ~linear:true constraints in
	if debug then Format.printf "Constraint:@.  %a@." Term.pr_list coeff_constrs;
	
        (* solve constraints and obtain coefficients of a ranking function *)
	let coefficients =
          try
            PolyConstrSolver.solve (Formula.band coeff_constrs)
          with PolyConstrSolver.Unknown ->
	    Format.printf "Failed to solve the constraints...@.@.";
            assert false(* failed to solve the constraints *)
	in
	let ((correspondence_, const_part) as ranking_function) = LinIntTermExp.of_term (Term.subst (List.map (fun (v, c) -> (v, Term.make_const (Const.Int c))) coefficients) linear_template)
	in
	let correspondence =
	  let cor dict x =
	    try List.assoc x dict with Not_found -> 0 in
	  let formatter (n, v) = (v, IntTerm.int_of n) in
	  List.map (cor (List.map formatter correspondence_)) arg_vars
	in
        (* update predicate *)
	let new_predicate_info =
	  BRA_types.updated_predicate_info
	    predicate_info
	    ({BRA_types.coeffs = correspondence; BRA_types.constant = IntTerm.int_of const_part} :: predicate_info.BRA_types.coefficients)
	    (spc :: predicate_info.BRA_types.error_paths)
	in
	if debug then Format.printf "\nInferred coefficients:@.  %a@." PolyConstrSolver.pr_coeffs coefficients;
	let _ = Queue.push new_predicate_info predicate_que in
	run predicate_que holed
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
	  let linear_templates = Fpat.ExtList.List.mapi (fun i _ -> unwrap_template (PolyConstrSolver.gen_template arg_env)) error_paths in
	  let linear_templates_prev = Fpat.ExtList.List.mapi (fun i lt -> Term.subst (List.combine arg_vars prev_var_terms) lt) linear_templates in
	  if debug then Fpat.ExtList.List.iteri (fun i lt -> Format.printf "Linear template(%d):@.  %a@." i Term.pr lt) linear_templates;
	  (** 作るべき制約は、
	      [φ1 ⇒ R1(x_prev)＞R1(x) ∧ R1(x)≧0]
	      ∧ [φ2 ⇒ R1(x_prev)≧R1(x) ∧ R2(x_prev)＞R2(x) ∧ R2(x)≧0]
	      ∧ [φ3 ⇒ R1(x_prev)≧R1(x) ∧ R2(x_prev)＝R2(x) ∧ R3(x_prev)＞R3(x) ∧ R3(x)≧0]
	      ...
	  **)
	  let all_vars = List.map fst env in
	  let subst_ith i = Term.subst (List.combine all_vars (List.map (fun v -> Term.make_var (Var.rename_base (fun (Idnt.Id v) -> Idnt.Id (v ^ "_IN_" ^ string_of_int i ^ "TH_ERRORPATH")) v)) all_vars)) in
	  let nth_constraints n nth_error_path =
	    let rec go i = 
	      let ith_ltp, ith_lt = List.nth linear_templates_prev i, List.nth linear_templates i in
	      if i < n then
		Formula.geq ith_ltp ith_lt :: go (i+1)
	      else
		[Formula.gt ith_ltp ith_lt; Formula.geq ith_lt (IntTerm.make 0)]
	    in
	    subst_ith n (imply nth_error_path (Formula.band (go 0)))
	  in
	  let constraints = Formula.bor (Fpat.ExtList.List.mapi nth_constraints error_paths)in
	  let coeff_constrs = PolyConstrSolver.generate ~linear:true constraints in
	  if debug then Format.printf "Constraint:@.  %a@." Term.pr constraints;
	  if debug then Format.printf "Constraints(Transformed by PolyConstrSolver.generate):@.  %a@." Term.pr_list coeff_constrs;
	  
	  try
	    (** solve constraints and obtain coefficients **)
	    let coefficients = PolyConstrSolver.solve (Formula.band coeff_constrs) in
	    if coefficients = [] then (Format.printf "@.Invalid ordered.@."; raise PolyConstrSolver.Unknown);
	    if debug then Format.printf "@.Inferred coefficients:@.  %a@." PolyConstrSolver.pr_coeffs coefficients;

	    let coefficient_infos = Fpat.ExtList.List.mapi (fun i ith_lt ->
	      let ((correspondence_, const_part) as ranking_function) = LinIntTermExp.of_term (Term.subst (List.map (fun (v, c) -> (v, Term.make_const (Const.Int c))) coefficients) ith_lt)
	      in
	      (** extract coefficients **)
	      let correspondence =
		let cor dict x =
		  try List.assoc x dict with Not_found -> 0 in
		let formatter (n, v) = (v, IntTerm.int_of n) in
		List.map (cor (List.map formatter correspondence_)) arg_vars
	      in
	      {BRA_types.coeffs = correspondence; BRA_types.constant = IntTerm.int_of const_part}
	    ) linear_templates
	    in
            (** return new predicate infomation (coeffcients + error paths) **)
	    let new_predicate_info =
	      BRA_types.updated_predicate_info
		predicate_info
		coefficient_infos
		error_paths
	    in
	    if debug then Format.printf "@.Found ranking function: %a@." BRA_types.pr_ranking_function new_predicate_info;
	    [new_predicate_info]
	  with PolyConstrSolver.Unknown ->
	    Format.printf "Failed to solve the constraints...@.@.";
            [] (* failed to solve the constraints *)
	) spc_sequences	
	in
	let _ = List.iter (fun pred -> Queue.push pred predicate_que) successes in
	run predicate_que holed
