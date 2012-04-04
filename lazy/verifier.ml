open ExtList
open ExtString

(** Verifier *)

let ext_coeffs = ref ([] : (Var.t * int) list)
let ext_constrs = ref ([] : Term.t list)

let init_coeffs prog =
		let cs = List.unique (Prog.coefficients prog) in
  let _ = Format.printf "parameters: %a@." (Util.pr_list Var.pr ",") cs in
		ext_coeffs := List.map (fun c -> c, 0) cs

let pr_coeffs ppf coeffs =
  let pr_aux ppf (c, n) = Format.fprintf ppf "%a = %d" Var.pr c n in
		Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_aux "@,") coeffs

let elim_univ_quantifiers t =
		let ts = Farkas.farkas t in
  let ts =
				List.map
					 (fun t ->
					   let _ = if !Global.debug > 1 then Format.printf "new constraint on coefficients:@.  @[<v>%a@]@." Term.pr t in
					   if !Global.use_bit_vector then
					     t
					   else
	         let t =
	           try
									     let t = Formula.simplify (Formula.linearize t) in
									     let _ = Format.printf "linearized constraint on coefficients:@.  @[<v>%a@]@." Term.pr t in
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
					     let _ = Format.printf "quantifier eliminated constraint on coefficients:@.  @[<v>%a@]@." Term.pr qft in
					     qft)
					 ts
		in
  ts

let solve_constrs () =
		if not !Global.use_bit_vector then
				List.filter (fun (c, _) -> Var.is_coeff c) (Cvc3Interface.solve (Formula.band !ext_constrs))
  else
	   let rec solve_aux only_pos_coeffs (* find only positive coefficients *) =
	     try
					   let t = Formula.band !ext_constrs in
								let t =
					     if only_pos_coeffs then
					       t
					     else
					       let ps = List.unique (Term.coefficients t) in
					       let ppps = List.map (fun (Var.C(id)) -> Var.C(id), Var.C(Idnt.make (Idnt.string_of id ^ "_pos")), Var.C(Idnt.make (Idnt.string_of id ^ "_neg"))) ps in
					       let sub = List.map (fun (x, y, z) -> x, Term.sub (Term.make_var y) (Term.make_var z)) ppps in
					       Term.subst (fun x -> List.assoc x sub) t
					   in
								let coeffs =
					     let t' =
            Formula.simplify
              (Term.subst
                (fun x ->
                  let n = List.assoc x !ext_coeffs in
                  if n = 0 then
                    raise Not_found
                  else (* reuse old solution if possible *)
                    Term.tint (n))
                t)
          in
										let t' = Formula.elim_minus t' in
										let _ = Format.printf "constraint on coefficients:@.  @[<v>%a@]@." Term.pr t' in
					     try
					       List.filter (fun (c, _) -> Var.is_coeff c) (Cvc3Interface.solve_bv t')
					     with Cvc3Interface.Unsatisfiable ->
					  					let t = Formula.elim_minus t in
							  			let _ = Format.printf "constraint on coefficients:@.  @[<v>%a@]@." Term.pr t in
					       List.filter (fun (c, _) -> Var.is_coeff c) (Cvc3Interface.solve_bv t)
					   in
					   if only_pos_coeffs then
					     coeffs
					   else
					     let pcs, ncs =
					       Util.partition_map
					         (fun (Var.C(id), n) ->
					           let s = Idnt.string_of id in
					           if String.ends_with s "_pos" then
					             `L(Var.C(Idnt.make (String.sub s 0 (String.length s - 4))) , n)
					           else if String.ends_with s "_neg" then
					             `R(Var.C(Idnt.make (String.sub s 0 (String.length s - 4))) , n)
					           else
					             assert false)
					         coeffs
					     in
					     let _ = if !Global.debug > 0 then assert (List.length pcs = List.length ncs) in
	  		     List.map (fun (c, n) -> c, n - try List.assoc c ncs with Not_found -> assert false) pcs
	     with Cvc3Interface.Unsatisfiable ->
	       if only_pos_coeffs then
	         solve_aux false
	       else
	         raise Cvc3Interface.Unsatisfiable
	   in
	   solve_aux true

let refine_coeffs t =
  let b =
			 let t' = Formula.simplify (Term.subst (fun x -> Term.tint (List.assoc x !ext_coeffs)) t) in
			 let _ = if !Global.debug > 1 then Format.printf "reuse old solution if:@.  @[<v>%a |= bot@]@." Term.pr t' in
    Cvc3Interface.is_valid (Formula.bnot t')
  in
		if b then
			 Format.printf "solutions (not changed):@.  %a@." pr_coeffs !ext_coeffs
		else
    let ts = elim_univ_quantifiers t in
				let _ = ext_constrs := ts @ !ext_constrs in
				let coeffs = solve_constrs () in
				let _ = ext_coeffs := coeffs @ List.filter (fun (c, _) -> not (List.mem_assoc c coeffs)) !ext_coeffs in
			 Format.printf "solutions:@.  %a@." pr_coeffs !ext_coeffs

let infer_ref_types prog etrs =
  let sums =
    if false then
      (* deprecated: old buggy refinement type inference algorithm *)
      let constrss = List.map TcGenRefType.cgen etrs in
      let _ = if !Global.debug > 1 then Format.printf "constraints:@.  @[<v>%a@]@." (Util.pr_list TraceConstr.pr "@,") constrss in
      TcSolveRefType.summaries_of (Prog.type_of prog) constrss
    else
      (* refinement type inference using Horn clauses *)
      let ctrs, hcss = List.split (List.map (HcGenRefType.cgen (Prog.type_of prog)) etrs) in
      let hcs = List.concat hcss in
      let _ = Format.printf "call trees:@.  @[<v>%a@]@." (Util.pr_list CallTree.pr "@,") ctrs in
      let _ = Format.printf "horn clauses:@.  @[<v>%a@]@." (Util.pr_list HornClause.pr "@,") hcs in
      let hcs =
						  if Util.concat_map HornClause.coefficients hcs = [] then
						    hcs
						  else
						    let t = HornClause.formula_of hcs in
						    let _ = Format.printf "verification condition:@.  @[<v>%a |= bot@]@." Term.pr t in
						    let _ = refine_coeffs t in
								  List.map (HornClause.subst (fun x -> Term.tint (List.assoc x !ext_coeffs))) hcs
      in
      List.map (fun (x, (_, t)) -> `P(x, t)) (HcSolve.solve ctrs hcs)
  in
  let _ =
    if !Global.debug > 1 then
      List.iter (fun (`P(x, t)) -> Format.printf "P[%a]: %a@." Var.pr x Term.pr t) sums
  in
  let fcs = List.unique (Util.concat_map Trace.function_calls_of etrs) in
  TcSolveRefType.infer_env prog sums fcs

let infer_int_types prog etrs =
  let constrss = List.map TcGenIntType.cgen etrs in
  let _ = if !Global.debug > 1 then Format.printf "constraints:@.  @[<v>%a@]@." (Util.pr_list TraceConstr.pr "@,") constrss in
  let sums = Util.concat_map
    (fun constrs ->
      Format.printf "@.";
      TcSolveIntType.summaries_of (Prog.type_of prog) constrs)
    constrss
  in
  let _ =
    if !Global.debug > 1 then
				  List.iter
		      (function `Pre((x, uid), pre) ->
						    Format.printf "Pre(%a,%d): %a@." Var.pr x uid Term.pr pre
						  | `Post((x, uid), post) ->
						    Format.printf "Post(%a,%d): %a@." Var.pr x uid Term.pr post)
        sums
  in
  let fcs = List.unique (Util.concat_map Trace.function_calls_of etrs) in
  TcSolveIntType.infer_env prog sums fcs

let infer_abst_type prog etrs =
  let env =
		  match Global.refine with
		    `RefType ->
		      let env = infer_ref_types prog etrs in
		      let _ = Format.printf "refinement types:@.  %a@." RefType.pr_env env in
		      List.map (fun (f, sty) -> f, AbsType.of_refinement_type sty) env
		  | `IntType ->
		      let env = infer_int_types prog etrs in
		      let _ = Format.printf "interaction types:@.  %a@." IntType.pr_env env in
		      let env = List.map (fun (f, sty) -> f, RefType.of_interaction_type sty) env in
		      let _ = Format.printf "refinement types:@.  %a@." RefType.pr_env env in
		      List.map (fun (f, sty) -> f, AbsType.of_refinement_type sty) env
		      (*
		      List.map (fun (f, sty) -> f, AbsType.of_interaction_type f sty) env
		      *)
  in
  List.map
    (function ((f, sty)::fstys) -> f, AbsType.merge (sty::List.map snd fstys) | _ -> assert false)
    (Util.classify (fun (f1, _) (f2, _) -> f1 = f2) env)

let refine cexs prog =
  try
    let _ = if !Global.debug > 0 then Format.printf "%a" Prog.pr prog in
    let etrs =
		    let cexs = [List.hd cexs](*???*) in
		    let _ = List.iter (fun cex -> Format.printf "%s@." (String.concat ":" (List.map string_of_int cex))) cexs in
		    let rt = CompTree.init prog in
		    let filter cts = List.filter (fun ct -> List.exists (Util.is_prefix ct.CompTree.path) cexs) cts in
		    let strategy = CompTreeExpander.filter_strategy filter rt in
		    let _ = CompTreeExpander.expand_all(*expand_until_new_error_trace_found*) prog rt strategy in
		    CompTree.error_traces_of rt
    in
    let _ = if !Global.debug > 0 then
      Format.printf "error traces:@.";
      List.iter (fun ep -> Format.printf "  %a@." Trace.pr ep) etrs
    in
    let env = infer_abst_type prog etrs in
    let _ = Format.printf "abstraction types:@.  %a@." AbsType.pr_env env in
    env
  with Util.NotImplemented s ->
    let _ = Format.printf "not implemented in %s@." s in
    assert false







let verify prog =
  let _ = if !Global.debug > 0 then Format.printf "%a" Prog.pr prog in
  try
    let rt = CompTree.init prog in
    let strategy = if true then CompTreeExpander.bf_strategy rt else CompTreeExpander.df_strategy rt in
    let rec loop old_etrs i =
      let _ = CompTreeExpander.expand_until_new_error_trace_found prog rt strategy in
      let etrs = CompTree.error_traces_of rt in
      let etrs' = Util.diff etrs old_etrs in
      let _ = if !Global.debug > 0 then
        Format.printf "error traces:@.";
        List.iter (fun ep -> Format.printf "  %a@." Trace.pr ep) etrs'
      in
      match Global.refine with
        `RefType ->
          let env = infer_ref_types prog etrs(*etrs'*) in
          let _ = Format.printf "refinement types:@.  %a@." RefType.pr_env env in
          if RefTypeCheck.check_prog env prog then
            Format.printf "@.The program is safe@."
          else
            loop etrs (i + 1)
      | `IntType ->
          let env = infer_int_types prog etrs(*etrs'*) in
          let _ = Format.printf "interaction types:@.  %a@." IntType.pr_env env in
          if IntTypeCheck.check_prog env prog then
            Format.printf "@.The program is safe@."
          else
            loop etrs (i + 1)
    in
    loop [] 1
  with TraceConstr.FeasibleErrorTrace(eptr) ->
    Format.printf "@.The program is unsafe@.Error trace: %a@." TraceConstr.pr eptr
