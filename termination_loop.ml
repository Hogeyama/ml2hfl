let (|>) = BRA_util.(|>)

let mapOption (f : 'a -> 'b option) (lst : 'a list) =
  let rec go = function
    | [] -> []
    | x::xs ->
      match f x with
	| Some y -> y :: go xs
	| None -> go xs
  in go lst
let flip f x y = f y x

(***** information *****)

let lrf = ref []
let max_threshold = 15

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

let inferCoeffs argumentVariables linear_templates constraints =
  let debug = !Flag.debug_level > 0 in
  let open Fpat in
  (** solve constraint and obtain coefficients **)
  let correspondenceVars = constraints |> RankFunInfer.generate |> Formula.band |> RankFunInfer.solve in
  begin
    if correspondenceVars = [] then (Format.printf "Invalid ordered.@."; raise PolyConstrSolver.Unknown);
    if debug then Format.printf "Inferred coefficients:@.  %a@." PolyConstrSolver.pr_coeffs correspondenceVars;
    
    List.map (fun linTemp ->
      (* EXAMPLE: ([v1 -> 1][v2 -> 0][v3 -> 1]..., v0=2) *)
      let ((correspondenceCoeffs, const_part) as ranking_function) = LinIntTermExp.of_term (Term.subst (List.map (fun (v, c) -> (v, Term.make_const (Const.Int c))) correspondenceVars) linTemp)
      in
      (** extract coefficients **)
      let coefficients =
	let cor dict x =
	  try List.assoc x dict with Not_found -> 0 in
	let formatter (n, v) = (v, IntTerm.int_of n) in
	List.map (cor (List.map formatter correspondenceCoeffs)) argumentVariables
      in
      {BRA_types.coeffs = coefficients; BRA_types.constant = IntTerm.int_of const_part}
    ) linear_templates
  end

let makeLexicographicConstraints variables linearTemplates prevLinearTemplates failedSpc spcSequence =
  (** make a constraint:
         [spc1 => R1(x_prev) > R1(x) /\ R1(x) >= 0]
      /\ [spc2 => R1(x_prev) >= R1(x)
               /\ R2(x_prev) > R2(x) /\ R2(x) >= 0]
      /\ [spc3 => R1(x_prev) >= R1(x)
               /\ R2(x_prev) >= R2(x)
               /\ R3(x_prev) > R3(x) /\ R3(x) >= 0]
      ...
  **)
  let debug = !Flag.debug_level > 0 in
  let open Fpat in
  let lenSpcSequence = List.length spcSequence in
  let subst_ith i = Term.subst (List.map (fun v -> (v, Term.make_var (Var.rename_base (fun (Idnt.Id v) -> Idnt.Id (v ^ "_IN_" ^ string_of_int i ^ "TH_ERRORPATH")) v))) variables) in
  let nth_constraints n =
    let rec go i = 
      let ith_ltp, ith_lt = List.nth prevLinearTemplates i, List.nth linearTemplates i in
      if i < n then
	Formula.geq ith_ltp ith_lt :: go (i+1)
      else
	[Formula.gt ith_ltp ith_lt; Formula.geq ith_lt (IntTerm.make 0)]
    in
    subst_ith n (Formula.band [List.nth failedSpc n; Formula.bnot (Formula.band (go 0))])
  in
  Formula.bor (Util.List.unfold (fun i -> if i < lenSpcSequence then Some (nth_constraints i, i+1) else None) 0)

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
    (* result log update here *)
    lrf := BRA_util.update_assoc (Id.to_string holed.BRA_types.verified.BRA_types.id, !cycle_counter, predicate_info) !lrf;
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

	(* make a constraint: spc => R(x_prev) > R(x) && R(x) >= 0 *)
	let constraints =
	  Formula.band [spc; Formula.bnot
	    (Formula.band [ Formula.gt linear_template_prev linear_template
			  ; Formula.geq linear_template (IntTerm.make 0)])]
	in
	if debug then Format.printf "Constraint:@.  %a@." Term.pr constraints;
	
        (* solve constraints and obtain coefficients of a ranking function *)
	let newPredicateInfo =
	  try
	    let coefficientInfos = inferCoeffs arg_vars [linear_template] constraints in
	    (* update predicate *)
	    BRA_types.updated_predicate_info predicate_info (coefficientInfos @ predicate_info.BRA_types.coefficients) (spc :: predicate_info.BRA_types.error_paths)
          with PolyConstrSolver.Unknown ->
	    if debug then Format.printf "Failed to solve the constraints...@.@.";
	    
	    (* Failed to infer a new ranking predicate -> Update extra parameters *)
	    (** UPDATE [not implemented] **)
            
            raise FailedToFindLLRF (* failed to solve the constraints *)
	in
	let _ = Queue.push newPredicateInfo predicate_que in
	run predicate_que holed
      else
	(* all order constructed by <spc_1, spc_2, ..., spc_n> and newly obtained spc *)
	let allSpcSequences =
	  let rec go l = function
	    | [] -> [l@[spc]]
	    | x::xs as r -> (l@[spc]@r) :: go (l@[x]) xs
	  in go [] predicate_info.BRA_types.error_paths
	in
	let numberOfSpcSequences = List.length allSpcSequences in
	let allVars = List.map fst env in
	
	let successes = (flip mapOption) allSpcSequences (fun spcSequence ->
	  (* make templates (for arguments and previous arguments) *)
	  let linearTemplates = Util.List.unfold (fun i -> if i < numberOfSpcSequences then Some (unwrap_template (PolyConstrSolver.gen_template arg_env), i+1) else None) 0 in
	  let prevLinearTemplates = List.map (Term.subst (List.combine arg_vars prev_var_terms)) linearTemplates in
	  if debug then Fpat.ExtList.List.iteri (fun i lt -> Format.printf "Linear template(%d):@.  %a@." i Term.pr lt) linearTemplates;

	  (* make a constraint *)
	  let constraints = makeLexicographicConstraints allVars linearTemplates prevLinearTemplates spcSequence allSpcSequences in
	  if debug then Format.printf "Constraint:@.  %a@." Term.pr constraints;
	  
	  try
	    (* solve constraint and obtain coefficients *)
	    let coefficientInfos = inferCoeffs arg_vars linearTemplates constraints in

            (* return new predicate information (coeffcients + error paths) *)
	    let newPredicateInfo = BRA_types.updated_predicate_info predicate_info coefficientInfos spcSequence in
	    if debug then Format.printf "Found ranking function: %a@." BRA_types.pr_ranking_function newPredicateInfo;
	    Some newPredicateInfo
	  with _ (* | PolyConstrSolver.Unknown (TODO[kuwahara]: INVESTIGATE WHICH EXCEPTION IS CAPTURED) *) ->
	    if debug then Format.printf "Failed to solve the constraints...@.@.";

	    (* Failed to infer a new ranking predicate -> Update extra parameters *)
	    (** UPDATE [not implemented] **)
            
	    None (* failed to solve the constraints *)
	)
	in
	let _ = List.iter (fun pred -> Queue.push pred predicate_que) successes in
	run predicate_que holed
