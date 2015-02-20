open BRA_types
open BRA_transform

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

let preprocessForTerminationVerification = ref (fun (x : Syntax.typed_term) -> x)

exception FailedToFindLLRF
exception InferenceFailure

let counter = ref 0
let get_now () = (counter := !counter + 1; !counter - 1)
let reset_counter () = counter := 0

let cycle_counter = ref 0
let incr_cycle () = cycle_counter := !cycle_counter + 1
let reset_cycle () = cycle_counter := 0

let verify_with holed pred =
  let debug = !Flag.debug_level > 0 in
  (* combine holed program and predicate *)
  let transformed = pluging holed pred in
  if debug then Format.printf "[%d]@.%a@." (get_now ()) Print.term transformed;
  let orig, transformed = retyping transformed (BRA_state.type_of_state holed) in
  Main_loop.run orig transformed

let inferCoeffs argumentVariables linear_templates constraints =
  (* reduce to linear constraint solving *)
  let _ = Fpat.RankFunInfer.ext_generate := Fpat.PolyConstrSolver.gen_coeff_constr ~nat:false ~linear:true in
  try
  let debug = !Flag.debug_level > 0 in
  (** solve constraint and obtain coefficients **)
  let correspondenceVars = constraints |> Fpat.RankFunInfer.generate |> Fpat.RankFunInfer.solve in
  let _ = Fpat.RankFunInfer.ext_generate := Fpat.PolyConstrSolver.gen_coeff_constr ~nat:false ~linear:false in
  begin
    if correspondenceVars = [] then (Format.printf "Invalid ordered.@."; raise Fpat.PolyConstrSolver.Unknown);
    if debug then Format.printf "Inferred coefficients:@.  %a@." Fpat.PolyConstrSolver.pr correspondenceVars;

    List.map (fun linTemp ->
      (* EXAMPLE: ([v1 -> 1][v2 -> 0][v3 -> 1]..., v0=2) *)
      let ((correspondenceCoeffs, const_part) as ranking_function) =
        Fpat.LinTermIntExp.of_term
          (Fpat.Term.subst
             (List.map (fun (v, t) -> (v, t)) correspondenceVars) linTemp)
      in
      (** extract coefficients **)
      let coefficients =
	let cor dict x =
	  try List.assoc x dict with Not_found -> 0 in
	let formatter (n, v) = (v, Fpat.IntTerm.int_of n) in
	List.map (cor (List.map formatter correspondenceCoeffs)) argumentVariables
      in
      {coeffs = coefficients; constant = Fpat.IntTerm.int_of const_part}
    ) linear_templates
  end
  with
    | e ->
      begin
	let _ = Fpat.RankFunInfer.ext_generate := Fpat.PolyConstrSolver.gen_coeff_constr ~nat:false ~linear:false in
	raise e
      end

let inferCoeffsAndExparams argumentVariables linear_templates constraints =
  let debug = !Flag.debug_level > 0 in
  (** solve constraint and obtain coefficients **)
  let correspondenceVars = constraints |> Fpat.RankFunInfer.generate |> Fpat.RankFunInfer.solve in
  begin
    if correspondenceVars = [] then (Format.printf "Invalid ordered.@."; raise Fpat.PolyConstrSolver.Unknown);
    if debug then Format.printf "Inferred coefficients:@.  %a@." Fpat.PolyConstrSolver.pr correspondenceVars;

    (List.map (fun linTemp ->
      (* EXAMPLE: ([v1 -> 1][v2 -> 0][v3 -> 1]...[vn -> 0], v0=2) *)
      let correspondenceCoeffs, const_part =
        Fpat.LinTermIntExp.of_term
          (Fpat.Term.subst (List.map (fun (v, t) -> (v, t)) correspondenceVars) linTemp)
      in
      (** extract coefficients **)
      let coefficients =
	let cor dict x =
	  try List.assoc x dict with Not_found -> 0 in
	let formatter (n, v) = (v, Fpat.IntTerm.int_of n) in
	List.map (cor (List.map formatter correspondenceCoeffs)) argumentVariables
      in
      {coeffs = coefficients; constant = Fpat.IntTerm.int_of const_part}
     ) linear_templates,
     let correspondenceExparams =
       List.map (fun (v, t) ->
                 let n = Fpat.IntTerm.int_of t in (v |> Fpat.Idnt.string_of |> (flip Id.from_string) Type.TInt, Term_util.make_int n)) (List.filter (fun (v, _) -> v |> Fpat.Idnt.string_of |> CEGAR_syntax.isEX_COEFFS) correspondenceVars) in
     let substToVar = function
       | {Syntax.desc = Syntax.Var x} -> (try List.assoc x correspondenceExparams with Not_found -> if CEGAR_syntax.isEX_COEFFS x.Id.name then Term_util.make_int 0 else Term_util.make_var x)
       | t -> t
     in
     let preprocessForExparam e =
       let e = ExtraParamInfer.removeDummySubstitutions e in
       let _ = ExtraParamInfer.withExparam := e in
       BRA_transform.everywhere_expr substToVar e
     in preprocessForExparam
    )
  end

let makeLexicographicConstraints variables linearTemplates prevLinearTemplates failedSpc =
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
  let lenSpcSequence = List.length failedSpc in
  let subst_ith i =
    Fpat.Formula.subst
      (List.map
        (fun v -> (v, Fpat.Term.mk_var (Fpat.Idnt.rename_base (fun v -> v ^ "_IN_" ^ string_of_int i ^ "TH_ERRORPATH") v)))
        variables)
  in
  let nth_constraints n =
    let rec go i =
      let ith_ltp, ith_lt = List.nth prevLinearTemplates i, List.nth linearTemplates i in
      if i < n then
        (Fpat.NumAtom.geq Fpat.Type.mk_int ith_ltp ith_lt |> Fpat.Formula.of_atom) :: go (i+1)
      else
        [Fpat.NumAtom.gt Fpat.Type.mk_int ith_ltp ith_lt |> Fpat.Formula.of_atom;
         Fpat.NumAtom.geq Fpat.Type.mk_int ith_lt (Fpat.IntTerm.make 0) |> Fpat.Formula.of_atom]
    in
    subst_ith n (Fpat.Formula.band [List.nth failedSpc n; Fpat.Formula.bnot (Fpat.Formula.band (go 0))])
  in
  Fpat.Formula.bor (Fpat.Util.List.unfold (fun i -> if i < lenSpcSequence then Some (nth_constraints i, i+1) else None) 0)

let rec enqueueNextPredicateInfo que =
  if Queue.is_empty que then None
  else
    let nextGen = Queue.pop que in
    try Some (nextGen ()) with _ -> enqueueNextPredicateInfo que

let rec run predicate_que holed =
  let debug = !Flag.debug_level > 0 in
  let _ =
    begin
      incr_cycle ();
      if !cycle_counter > max_threshold then (raise FailedToFindLLRF)
    end
  in
  match enqueueNextPredicateInfo predicate_que with
    | None -> (raise FailedToFindLLRF)
    | Some predicate_info ->
      (* result log update here *)
      lrf := BRA_util.update_assoc (Id.to_string holed.verified.id, !cycle_counter, predicate_info) !lrf;

      (* set subst. to coeffs. of exparams (use in Main_loop.run as preprocess) *)
      BRA_types.preprocessForTerminationVerification := predicate_info.substToCoeffs;

      try
	let result = if !Flag.separate_pred then
	    let predicates = separate_to_CNF (construct_LLRF predicate_info) in
            List.for_all (verify_with holed) predicates
	  else if !Flag.split_callsite then
	    let predicate = construct_LLRF predicate_info in
	    let splited = callsite_split holed in
	    reset_counter ();
	    List.for_all (fun h -> verify_with h predicate) splited
	  else
	    let predicate = construct_LLRF predicate_info in
	    verify_with holed predicate
	in
	if result then
	  (if debug then Format.printf "%s is terminating.@." holed.verified.id.Id.name ; result)
	else
	  (if debug then Format.printf "%s is possibly non-terminating.@." holed.verified.id.Id.name ; result)
      with Refine.PostCondition (env, spc, spcWithExparam) ->
	let unwrap_template (Fpat.Term.App (Fpat.Term.App (_, t), _)) = t in
	let unwrap_template t = unwrap_template (Fpat.Formula.term_of t) in
	let arg_vars =
	  List.map (fun v -> Fpat.Idnt.make (Id.to_string (extract_id v)))
	    (BRA_state.get_argvars holed.state holed.verified) in
	let prev_vars =
	  List.map (fun v -> Fpat.Idnt.make (Id.to_string (extract_id v)))
	    (BRA_state.get_prev_statevars holed.state holed.verified) in
	let prev_var_terms = List.map Fpat.Term.mk_var prev_vars in
	let arg_env = List.map (fun a -> (a, Fpat.Type.mk_int)) arg_vars in

	if !Flag.disjunctive then
	  (* make templates *)
	  let linear_template = unwrap_template (Fpat.Template.mk_atom arg_env) in
	  let linear_template_prev = Fpat.Term.subst (List.combine arg_vars prev_var_terms) linear_template in
	  if debug then Format.printf "Linear template:@.  %a@." Fpat.Term.pr linear_template;

	  (* make a constraint: spc => R(x_prev) > R(x) && R(x) >= 0 *)
	  let constraints =
	    Fpat.Formula.band [spc; Fpat.Formula.bnot
	      (Fpat.Formula.band [ Fpat.NumAtom.gt Fpat.Type.mk_int linear_template_prev linear_template |> Fpat.Formula.of_atom
			    ; Fpat.NumAtom.geq Fpat.Type.mk_int linear_template (Fpat.IntTerm.make 0) |> Fpat.Formula.of_atom])]
	  in
	  if debug then Format.printf "Constraint:@.  %a@." Fpat.Formula.pr constraints;

          (* solve constraints and obtain coefficients of a ranking function *)
	  let newPredicateInfo _ =
	    try
	      let coefficientInfos = inferCoeffs arg_vars [linear_template] constraints in
	      (* return updated predicate *)
	      { predicate_info with coefficients = coefficientInfos @ predicate_info.coefficients; errorPaths = spc :: predicate_info.errorPaths }
            with Fpat.PolyConstrSolver.Unknown ->
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
	    in go [] predicate_info.errorPaths
	  in
	  let allSpcSequencesWithExparam =
	    let rec go l = function
	      | [] -> [l@[spcWithExparam]]
	      | x::xs as r -> (l@[spcWithExparam]@r) :: go (l@[x]) xs
	    in go [] predicate_info.errorPathsWithExparam
	  in
	  let numberOfSpcSequences = List.length allSpcSequences in
	  let allVars = List.map fst env in

	  let successes = (flip List.map) (List.combine allSpcSequences allSpcSequencesWithExparam) (fun (spcSequence, spcSequenceWithExparam) -> fun _ ->
	    (* make templates (for arguments and previous arguments) *)
	    let linearTemplates = Fpat.Util.List.unfold (fun i -> if i < numberOfSpcSequences then Some (unwrap_template (Fpat.Template.mk_atom arg_env), i+1) else None) 0 in
	    let prevLinearTemplates = List.map (Fpat.Term.subst (List.combine arg_vars prev_var_terms)) linearTemplates in
	    if debug then Fpat.Util.List.iteri (fun i lt -> Format.printf "Linear template(%d):@.  %a@." i Fpat.Term.pr lt) linearTemplates;	    try
	      (* make a constraint *)
	      let constraints = makeLexicographicConstraints allVars linearTemplates prevLinearTemplates spcSequence in
	      if debug then Format.printf "Constraint:@.  %a@." Fpat.Formula.pr constraints;

	      (* solve constraint and obtain coefficients *)
	      let coefficientInfos = inferCoeffs arg_vars linearTemplates constraints in

              (* return new predicate information (coeffcients + error paths) *)
	      let newPredicateInfo = { predicate_info with coefficients = coefficientInfos; errorPaths = spcSequence; errorPathsWithExparam = spcSequenceWithExparam} in
	      if debug then Format.printf "Found ranking function: %a@." pr_ranking_function newPredicateInfo;
	      newPredicateInfo
	    with _ (* | Fpat.PolyConstrSolver.Unknown (TODO[kuwahara]: INVESTIGATE WHICH EXCEPTION IS CAPTURED) *) ->
	      if debug then Format.printf "Try to update extra parameters...@.@.";

	      try
		(* make a constraint *)
		let constraints = makeLexicographicConstraints allVars linearTemplates prevLinearTemplates spcSequenceWithExparam in
		if debug then Format.printf "Constraint:@.  %a@." Fpat.Formula.pr constraints;

		(* solve constraint and obtain coefficients *)
		let coefficientInfos, exparamInfo = inferCoeffsAndExparams arg_vars linearTemplates constraints in

		(* return new predicate information (coeffcients + error paths) *)
		let newPredicateInfo = { predicate_info with coefficients = coefficientInfos; substToCoeffs = exparamInfo; errorPaths = spcSequence; errorPathsWithExparam = spcSequenceWithExparam } in
		if debug then Format.printf "Found ranking function: %a@." pr_ranking_function newPredicateInfo;
		newPredicateInfo
              with
		| Fpat.PolyConstrSolver.NoSolution ->
		  if debug then Format.printf "Failed to find LLRF...@.";
		  raise InferenceFailure (* failed to solve the constraints *)
		| e ->
		  if debug then Format.printf "error: %s@." (Printexc.to_string e);
		  raise InferenceFailure (* failed to solve the constraints *)
	  )
	  in
	  let _ = List.iter (fun pred -> Queue.push pred predicate_que) successes in
	  run predicate_que holed
