open ExtList
open ExtString

(** Verifier *)

let ext_coeffs = ref ([] : (Var.t * int) list)
let ext_constrs = ref ([] : Term.t list)

let init_coeffs prog =
		let cs = List.unique (Prog.coeffs prog) in
  let _ = Format.printf "parameters: %a@," Var.pr_list cs in
		ext_coeffs := List.map (fun c -> c, 0) cs

let pr_coeffs ppf coeffs =
  let pr_aux ppf (c, n) = Format.fprintf ppf "%a = %d" Var.pr c n in
		Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_aux "@,") coeffs

let elim_univ_quantifiers t =
  let _ = Global.log_begin "elim_univ_quantifiers" in
		let ts = Farkas.farkas t in
  let ts =
				List.map
					 (fun t ->
					   let _ = Global.log (fun () -> Format.printf "new constraint on coefficients:@,  @[<v>%a@]@," Term.pr t) in
					   if !Global.use_bit_vector then
					     t
					   else
	         let t =
	           try
									     let t = Formula.simplify (Formula.linearize t) in
									     let _ = Format.printf "linearized constraint on coefficients:@,  @[<v>%a@]@," Term.pr t in
			           t
	           with Invalid_argument _ ->
	             t
	         in
					     let qft =
					       if Formula.is_linear t then
					         Formula.simplify (AtpInterface.integer_qelim t)
					       else
					         Formula.simplify (AtpInterface.real_qelim t)
					     in
					     let _ = Format.printf "quantifier eliminated constraint on coefficients:@,  @[<v>%a@]@," Term.pr qft in
					     qft)
					 ts
		in
  let _ = Global.log_end "elim_univ_quantifiers" in
  ts

exception FailedToRefineExtraParameters

let solve_bv_aux t =
  let ibs = List.init !Global.bits_threshold (fun i -> i + 1, true) @ List.init (!Global.bits_threshold - 1(*???*)) (fun i -> i + 1, false) in
		try
    Util.find_map (fun (bit, only_pos) -> try Cvc3Interface.solve_bv only_pos bit t with Cvc3Interface.Unknown -> raise Not_found) ibs
		with Not_found ->
    raise Cvc3Interface.Unknown

let masked_params : Var.t list ref = ref []

let solve_bv t =
  let _ = Global.log_begin "Verifier.solve_bv" in
  let sol =
				try
				  if !Global.disable_parameter_inference_heuristics then
						  raise Cvc3Interface.Unknown
						else
						  let masked_params = Util.inter !masked_params (Term.coeffs t) in
								if masked_params = [] then
								  raise Cvc3Interface.Unknown
								else
										let _ = Global.log (fun () -> Format.printf "masked_params: %a@," Var.pr_list masked_params) in
										let coeffs = List.map (fun c -> c, 0) masked_params in
										let t' = Formula.simplify (Term.subst (fun x -> Term.tint (List.assoc x coeffs)) t) in
										coeffs @ solve_bv_aux t'
				with Cvc3Interface.Unknown ->
		  		solve_bv_aux t
  in
  let _ = Global.log_end "Verifier.solve_bv" in
  sol

let solve_constrs t =
		if not !Global.use_bit_vector then
				List.filter (fun (c, _) -> Var.is_coeff c) (Cvc3Interface.solve t)
  else
				let changed = ref false in
				let t' =
      Formula.simplify
        (Term.subst
          (fun x ->
            let n = List.assoc x !ext_coeffs in
            if n = 0 then
              raise Not_found
            else (* reuse old solution if possible *)
														let _ = changed := true in
              Term.tint (n))
          t)
    in
				let _ = Global.log (fun () -> Format.printf "solving a constraint on coefficients (reusing old solution):@,  @[<v>%a@]@," Term.pr t') in
				try
						List.filter (fun (c, _) -> Var.is_coeff c) (solve_bv t')
				with Cvc3Interface.Unknown ->
						if not !changed then
								raise FailedToRefineExtraParameters
						else
						  if true then
  								raise FailedToRefineExtraParameters
								else
								  (*a-test-update fails*)
										let _ = Global.log (fun () -> Format.printf "solving a constraint on coefficients:@,  @[<v>%a@]@," Term.pr t) in
										try
		  								List.filter (fun (c, _) -> Var.is_coeff c) (solve_bv t)
										with Cvc3Interface.Unknown ->
										  raise FailedToRefineExtraParameters

let elapsed_time = ref 0.0
let refine_coeffs hcs =
  let _ = Global.log_begin "refine_coeffs" in
  let timer = Global.timer () in
  (*
		let t = if !Global.fol_backward then HcSolve.formula_of_backward hcs else HcSolve.formula_of_forward_ext hcs in
		let _ = Global.log (fun () -> Format.printf "verification condition:@,  @[<v>%a |= bot@]@," Term.pr t) in
		let b =
			 let t' = Formula.simplify (Term.subst (fun x -> Term.tint (List.assoc x !ext_coeffs)) t) in
			 let _ = Global.log (fun () -> Format.printf "reuse old solution if:@,  @[<v>%a |= bot@]@," Term.pr t') in
    Cvc3Interface.is_valid (Formula.bnot t')
  in
		*)
		let b =
		  let hcs = 
						let hcs = List.map (HornClause.subst (fun x -> Term.tint (List.assoc x !ext_coeffs))) hcs in
						let hcs1, hcs2 = List.partition (function HornClause.Hc(Some(pid, _), _, _) -> Var.is_coeff pid | _ -> false) hcs in
						List.map (HornClauseEc.subst_hcs(*_fixed*) hcs1) hcs2
				in
  		let t = if !Global.fol_backward then HcSolve.formula_of_backward hcs else HcSolve.formula_of_forward_ext hcs in
			 let _ = Global.log (fun () -> Format.printf "reuse old solution if:@,  @[<v>%a |= bot@]@," Term.pr t) in
    Cvc3Interface.is_valid (Formula.bnot t)
		in
  let _ =
				if b then
					 Global.log (fun () -> Format.printf "solutions (not changed):@,  %a@," pr_coeffs !ext_coeffs)
				else
						let t = if !Global.fol_backward then HcSolve.formula_of_backward hcs else HcSolve.formula_of_forward_ext hcs in
						let _ = Global.log (fun () -> Format.printf "verification condition:@,  @[<v>%a |= bot@]@," Term.pr t) in
		    let ts = elim_univ_quantifiers t in
						let _ = ext_constrs := ts @ !ext_constrs in
						let coeffs = solve_constrs (if not !Global.accumulate_ext_constrs then Formula.band ts else Formula.band !ext_constrs) in
						let _ = ext_coeffs := coeffs @ List.filter (fun (c, _) -> not (List.mem_assoc c coeffs)) !ext_coeffs in
					 Global.log (fun () -> Format.printf "solutions:@,  %a@," pr_coeffs !ext_coeffs)
  in
		let _ = Cvc3Interface.close_cvc3 (); Cvc3Interface.open_cvc3 () in
		let _ = elapsed_time := timer () in
  Global.log_end "refine_coeffs"

let infer_env prog sums fcs =
  let env = RefType.of_summaries prog sums fcs in
  let env' =
    List.map
      (fun (f, sty) ->
        f, RefType.of_simple_type sty)
      (List.find_all
        (fun (f, sty) -> not (List.mem_assoc f env))
        (List.map
          (fun (f, sty) -> Var.make f, sty)
          prog.Prog.types))
  in
  env @ env'

let infer_ref_types fs prog etrs =
  let _ = Global.log_begin "infer_ref_types" in
  let sums =
    (*if false then
      (* deprecated: old buggy refinement type inference algorithm *)
      let constrss = List.map TcGenRefType.cgen etrs in
      let _ = Global.log (fun () -> Format.printf "constraints:@,  @[<v>%a@]@," (Util.pr_list TraceConstr.pr "@,") constrss) in
      TcSolveRefType.summaries_of (Prog.type_of prog) constrss
    else*)
      (* refinement type inference using Horn clauses *)
      let ctrs, hcss = List.split (List.map (HcGenRefType.cgen (Prog.type_of prog)) etrs) in
      let hcs = List.concat hcss in
      let _ = Global.log (fun () -> Format.printf "call trees:@,  @[<v>%a@]@," (Util.pr_list CallTree.pr "@,") ctrs) in
      let hcs = List.map (HornClauseEc.simplify []) hcs in
      let _ = Global.log (fun () -> Format.printf "horn clauses:@,  %a@," HornClause.pr hcs) in
      let orig_hcs = hcs in
      let inline pid =
        not (Var.is_coeff pid) &&
        List.exists
          (fun f ->
            let Var.V(id), _ = CallId.tlfc_of pid in
            Idnt.string_of id = f)
          fs
      in
      let hcs =
        if !Global.no_inlining || !Global.inline_after_ncs then
          hcs
        else
          let hcs = HcSolve.inline_forward inline hcs in
          let _ = Global.log (fun () -> Format.printf "inlined horn clauses:@,  %a@," HornClause.pr hcs) in
          hcs
      in
      let hcs, orig_hcs =
						  if Util.concat_map HornClause.coeffs hcs = [] then
						    hcs, orig_hcs
						  else
						    let _ = refine_coeffs hcs in
										let _ = Format.printf "inferred extra parameters:@,  %a@," pr_coeffs !ext_coeffs in
								  let hcs = List.map (HornClause.subst (fun x -> Term.tint (List.assoc x !ext_coeffs))) hcs in
								  let hcs1, hcs2 = List.partition (function HornClause.Hc(Some(pid, _), _, _) -> Var.is_coeff pid | _ -> false) hcs in
								  List.map (HornClauseEc.subst_hcs(*_fixed*) hcs1) hcs2,

								  let orig_hcs = List.map (HornClause.subst (fun x -> Term.tint (List.assoc x !ext_coeffs))) orig_hcs in
								  let orig_hcs1, orig_hcs2 = List.partition (function HornClause.Hc(Some(pid, _), _, _) -> Var.is_coeff pid | _ -> false) orig_hcs in
								  List.map (HornClauseEc.subst_hcs(*_fixed*) orig_hcs1) orig_hcs2
      in
      let _ =
						  if Util.concat_map HornClause.coeffs hcs <> [] then
								  Global.log
										  (fun () ->
												  Format.printf
														  "non-parametrized horn clauses:@,  %a@,"
																HornClause.pr hcs)
						in
      let hcs =
        if !Global.no_inlining || not !Global.inline_after_ncs then
          hcs
        else
          let hcs = HcSolve.inline_forward inline hcs in
          let _ = Global.log (fun () -> Format.printf "inlined horn clauses:@,  %a@," HornClause.pr hcs) in
          hcs
      in
						let sol =
						  let sol =
          match !Global.predicate_discovery with
										  Global.ConvexHull -> HcGenSolve.solve true hcs
										| Global.TemplateBasedConstraintSolving -> HcGenSolve.solve false hcs
										| Global.Backward -> HcBwSolve.solve hcs
								in
								let _ =
						    if !Global.debug && false then
						      let orig_hcs = List.map (HornClauseEc.simplify []) (List.map (TypPredSubst.subst_lhs sol) orig_hcs) in
						      let lbs = HcSolve.compute_lbs orig_hcs in
						      let orig_hcs = List.map (HornClauseEc.simplify []) (List.map (TypPredSubst.subst_lhs lbs) orig_hcs) in
						      let _ = Global.log (fun () -> Format.printf "solved horn clauses:@,  %a@," HornClause.pr orig_hcs) in
						      let sol = sol @ List.filter_map (function HornClause.Hc(Some(pid, xtys), [], t) -> Some(pid, (xtys, t)) | _ -> None ) orig_hcs in
						      TypPredSubst.check sol orig_hcs
						  in
								sol
						in
      List.map
        (fun (x, (xtys, t)) ->
          let ytys = RefType.visible_vars (Prog.type_of prog) x in
          let _ = assert (List.length xtys = List.length ytys) in
          let sub = List.map2 (fun (x, _) (y, _) -> x, Term.make_var y) xtys ytys in
          `P(x, Term.subst (fun x -> List.assoc x sub) t))
        sol
  in
  let _ =
    if false then
		    Global.log
		      (fun () ->
						    let _ = Format.printf "summaries:@,  @[<v>" in
		        let _ = List.iter (fun (`P(x, t)) -> Format.printf "P[%a]: %a@," Var.pr x Term.pr t) sums in
						    Format.printf "@]@,")
  in
  let fcs = List.unique (Util.concat_map Trace.function_calls_of etrs) in
  let res = infer_env prog sums fcs in
  let _ = Global.log_end "infer_ref_types" in
  res


(*
let infer_int_types prog etrs =
  let _ = Global.log_begin "infer_int_types" in
  let constrss = List.map TcGenIntType.cgen etrs in
  let _ = Global.log (fun () -> Format.printf "constraints:@,  @[<v>%a@]@," (Util.pr_list TraceConstr.pr "@,") constrss) in
  let sums = Util.concat_map
    (fun constrs ->
      Format.printf "@,";
      TcSolveIntType.summaries_of (Prog.type_of prog) constrs)
    constrss
  in
  let _ =
    Global.log (fun () ->
				  List.iter
		      (function `Pre((x, uid), pre) ->
						    Format.printf "Pre(%a,%d): %a@," Var.pr x uid Term.pr pre
						  | `Post((x, uid), post) ->
						    Format.printf "Post(%a,%d): %a@," Var.pr x uid Term.pr post)
        sums)
  in
  let fcs = List.unique (Util.concat_map Trace.function_calls_of etrs) in
  let res = TcSolveIntType.infer_env prog sums fcs in
  let _ = Global.log_end "infer_int_types" in
  res
*)

let infer_abst_type fs prog etrs =
  let env =
		  match Global.refine with
		    `RefType ->
		      let env = infer_ref_types fs prog etrs in
		      let _ = if !Global.print_log then Format.printf "refinement types:@,  %a@," RefType.pr_env env in
		      List.map (fun (f, sty) -> f, AbsType.of_refinement_type sty) env
		  (*| `IntType ->
		      let env = infer_int_types prog etrs in
		      let _ = if !Global.print_log then Format.printf "interaction types:@,  %a@," IntType.pr_env env in
		      let env = List.map (fun (f, sty) -> f, RefType.of_interaction_type sty) env in
		      let _ = if !Global.print_log then Format.printf "refinement types:@,  %a@," RefType.pr_env env in
		      List.map (fun (f, sty) -> f, AbsType.of_refinement_type sty) env
		      (*
		      List.map (fun (f, sty) -> f, AbsType.of_interaction_type f sty) env
		      *)*)
  in
  List.map
    (function ((f, sty)::fstys) -> f, AbsType.merge (sty::List.map snd fstys) | _ -> assert false)
    (Util.classify (fun (f1, _) (f2, _) -> f1 = f2) env)

exception FailedToRefineTypes

let refine fs cexs prog =
  let _ = Global.log_begin "refine" in
  let _ = Global.log (fun () -> Format.printf "inlined functions: %s@," (String.concat "," fs)) in
  let env =
		  try
		    let _ = Global.log (fun () -> Format.printf "program:@,  %a@," Prog.pr prog) in
		    let etrs =
				    let _ = Global.log (fun () -> List.iter (fun cex -> Format.printf "counterexample: @[<v>%s@]@," (String.concat ":" (List.map string_of_int cex))) cexs) in
				    let rt = CompTree.init prog in
				    let filter cts = List.filter (fun ct -> List.exists (Util.is_prefix ct.CompTree.path) cexs) cts in
				    let strategy = CompTreeExpander.filter_strategy filter rt in
				    let _ = CompTreeExpander.expand_all(*expand_until_new_error_trace_found*) prog rt strategy in
				    CompTree.error_traces_of rt
		    in
		    let _ = Global.log (fun () -> Format.printf "error traces:@,  @[<v>%a@]@," (Util.pr_list Trace.pr "@,") etrs) in
		    let env = try infer_abst_type fs prog etrs with HcSolve.NoSolution -> raise FailedToRefineTypes in
		    let _ = if !Global.print_log then Format.printf "abstraction types:@,  %a@," AbsType.pr_env env in
		    env
		  with Util.NotImplemented s ->
		    let _ = Format.printf "not implemented in %s@," s in
		    assert false
  in
  let _ = Global.log_end "refine" in
  env







let verify fs prog =
  let _ = Global.log_begin "verify" in
  let _ = Global.log (fun () -> Format.printf "%a" Prog.pr prog) in
  let _ =
		  (*try*)
		    let rt = CompTree.init prog in
		    let strategy = if true then CompTreeExpander.bf_strategy rt else CompTreeExpander.df_strategy rt in
		    let rec loop old_etrs i =
		      let _ = CompTreeExpander.expand_until_new_error_trace_found prog rt strategy in
		      let etrs = CompTree.error_traces_of rt in
		      let etrs' = Util.diff etrs old_etrs in
		      let _ = Global.log (fun () ->
		        let _ = Format.printf "error traces:@," in
		        List.iter (fun ep -> Format.printf "  %a@," Trace.pr ep) etrs')
		      in
		      match Global.refine with
		        `RefType ->
		          let env = infer_ref_types fs prog etrs(*etrs'*) in
		          let _ = if !Global.print_log then Format.printf "refinement types:@,  %a@," RefType.pr_env env in
		          if RefTypeCheck.check_prog env prog then
		            Format.printf "@,The program is safe@,"
		          else
		            loop etrs (i + 1)
		      (*| `IntType ->
		          let env = infer_int_types prog etrs(*etrs'*) in
		          let _ = if !Global.print_log then Format.printf "interaction types:@,  %a@," IntType.pr_env env in
		          if IntTypeCheck.check_prog env prog then
		            Format.printf "@,The program is safe@,"
		          else
		            loop etrs (i + 1)*)
		    in
		    loop [] 1
		  (*with TraceConstr.FeasibleErrorTrace(eptr) ->
		    Format.printf "@,The program is unsafe@,Error trace: %a@," TraceConstr.pr eptr*)
  in
  let _ = Global.log_end "verify" in
  ()

