open ExtList
open ExtString

(** Verifier *)

let ext_coeffs = ref ([] : (Var.t * int) list)
let ext_constrs = ref ([] : Term.t list)

let init_coeffs prog =
		let cs = List.unique (Prog.coeffs prog) in
  let _ = Format.printf "parameters: %a@," (Util.pr_list Var.pr ",") cs in
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
					       let ps = List.unique (Term.coeffs t) in
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
										let _ = Format.printf "constraint on coefficients:@,  @[<v>%a@]@," Term.pr t' in
					     try
					       List.filter (fun (c, _) -> Var.is_coeff c) (Cvc3Interface.solve_bv t')
					     with Cvc3Interface.Unknown ->
					  					let t = Formula.elim_minus t in
							  			let _ = Format.printf "constraint on coefficients:@,  @[<v>%a@]@," Term.pr t in
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
					     let _ = assert (List.length pcs = List.length ncs) in
	  		     List.map (fun (c, n) -> c, n - try List.assoc c ncs with Not_found -> assert false) pcs
	     with Cvc3Interface.Unknown ->
	       if only_pos_coeffs then
	         solve_aux false
	       else
	         raise Cvc3Interface.Unknown
	   in
	   solve_aux true

let refine_coeffs t =
  let _ = Global.log_begin "refine_coeffs" in
  let b =
			 let t' = Formula.simplify (Term.subst (fun x -> Term.tint (List.assoc x !ext_coeffs)) t) in
			 let _ = Global.log (fun () -> Format.printf "reuse old solution if:@,  @[<v>%a |= bot@]@," Term.pr t') in
    Cvc3Interface.is_valid (Formula.bnot t')
  in
  let _ =
				if b then
					 Format.printf "solutions (not changed):@,  %a@," pr_coeffs !ext_coeffs
				else
		    let ts = elim_univ_quantifiers t in
						let _ = ext_constrs := ts @ !ext_constrs in
						let coeffs = solve_constrs () in
						let _ = ext_coeffs := coeffs @ List.filter (fun (c, _) -> not (List.mem_assoc c coeffs)) !ext_coeffs in
					 Format.printf "solutions:@,  %a@," pr_coeffs !ext_coeffs
  in
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
    if false then
      (* deprecated: old buggy refinement type inference algorithm *)
      let constrss = List.map TcGenRefType.cgen etrs in
      let _ = Global.log (fun () -> Format.printf "constraints:@,  @[<v>%a@]@," (Util.pr_list TraceConstr.pr "@,") constrss) in
      TcSolveRefType.summaries_of (Prog.type_of prog) constrss
    else
      (* refinement type inference using Horn clauses *)
      let ctrs, hcss = List.split (List.map (HcGenRefType.cgen (Prog.type_of prog)) etrs) in
      let hcs = List.concat hcss in
      let _ = Global.log (fun () -> Format.printf "call trees:@,  @[<v>%a@]@," (Util.pr_list CallTree.pr "@,") ctrs) in
      let _ = Global.log (fun () -> Format.printf "horn clauses:@,  @[<v>%a@]@," (Util.pr_list HornClause.pr "@,@,") hcs) in
      let hcs = if !Global.inline then HcSolve.inline_forward fs hcs else hcs in
      let _ = Global.log (fun () -> Format.printf "inlined horn clauses:@,  @[<v>%a@]@," (Util.pr_list HornClause.pr "@,@,") hcs) in
      let hcs =
						  if Util.concat_map HornClause.coeffs hcs = [] then
						    hcs
						  else
						    let t = HcSolve.formula_of_forward_ext hcs in
						    let _ = Global.log (fun () -> Format.printf "verification condition:@,  @[<v>%a |= bot@]@," Term.pr t) in
						    let _ = refine_coeffs t in
								  let hcs = List.map (HornClause.subst (fun x -> Term.tint (List.assoc x !ext_coeffs))) hcs in
								  let hcs1, hcs2 = List.partition (function HornClause.Hc(Some(pid, _), _, _) -> Var.is_coeff pid | _ -> false) hcs in
								  List.map (HornClause.subst_hcs_fixed hcs1) hcs2
      in
      List.map
        (fun (x, (xs, t)) ->
          let ys = RefType.visible_vars (Prog.type_of prog) x in
          let _ = assert (List.length xs = List.length ys) in
          let sub = List.map2 (fun x y -> x, Term.make_var y) xs ys in
          `P(x, Term.subst (fun x -> List.assoc x sub) t))
        (HcSolve.solve prog ctrs hcs)
  in
  let _ = Global.log (fun () -> List.iter (fun (`P(x, t)) -> Format.printf "P[%a]: %a@," Var.pr x Term.pr t) sums) in
  let fcs = List.unique (Util.concat_map Trace.function_calls_of etrs) in
  let res = infer_env prog sums fcs in
  let _ = Global.log_end "infer_ref_types" in
  res


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

let infer_abst_type fs prog etrs =
  let env =
		  match Global.refine with
		    `RefType ->
		      let env = infer_ref_types fs prog etrs in
		      let _ = Format.printf "refinement types:@,  %a@," RefType.pr_env env in
		      List.map (fun (f, sty) -> f, AbsType.of_refinement_type sty) env
		  | `IntType ->
		      let env = infer_int_types prog etrs in
		      let _ = Format.printf "interaction types:@,  %a@," IntType.pr_env env in
		      let env = List.map (fun (f, sty) -> f, RefType.of_interaction_type sty) env in
		      let _ = Format.printf "refinement types:@,  %a@," RefType.pr_env env in
		      List.map (fun (f, sty) -> f, AbsType.of_refinement_type sty) env
		      (*
		      List.map (fun (f, sty) -> f, AbsType.of_interaction_type f sty) env
		      *)
  in
  List.map
    (function ((f, sty)::fstys) -> f, AbsType.merge (sty::List.map snd fstys) | _ -> assert false)
    (Util.classify (fun (f1, _) (f2, _) -> f1 = f2) env)

let refine fs cexs prog =
  let _ = Global.log_begin "refine" in
  let _ = Global.log (fun () -> Format.printf "inlined functions: %s@," (String.concat "," fs)) in
  let env =
		  try
		    let _ = Global.log (fun () -> Format.printf "program:@,  %a@," Prog.pr prog) in
		    let etrs =
				    let cexs = [List.hd cexs](*???*) in
				    let _ = Global.log (fun () -> List.iter (fun cex -> Format.printf "counterexample: @[<v>%s@]@," (String.concat ":" (List.map string_of_int cex))) cexs) in
				    let rt = CompTree.init prog in
				    let filter cts = List.filter (fun ct -> List.exists (Util.is_prefix ct.CompTree.path) cexs) cts in
				    let strategy = CompTreeExpander.filter_strategy filter rt in
				    let _ = CompTreeExpander.expand_all(*expand_until_new_error_trace_found*) prog rt strategy in
				    CompTree.error_traces_of rt
		    in
		    let _ = Global.log (fun () -> Format.printf "error traces:@,  @[<v>%a@]@," (Util.pr_list Trace.pr "@,") etrs) in
		    let env = infer_abst_type fs prog etrs in
		    let _ = Format.printf "abstraction types:@,  %a@," AbsType.pr_env env in
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
		  try
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
		          let _ = Format.printf "refinement types:@,  %a@," RefType.pr_env env in
		          if RefTypeCheck.check_prog env prog then
		            Format.printf "@,The program is safe@,"
		          else
		            loop etrs (i + 1)
		      | `IntType ->
		          let env = infer_int_types prog etrs(*etrs'*) in
		          let _ = Format.printf "interaction types:@,  %a@," IntType.pr_env env in
		          if IntTypeCheck.check_prog env prog then
		            Format.printf "@,The program is safe@,"
		          else
		            loop etrs (i + 1)
		    in
		    loop [] 1
		  with TraceConstr.FeasibleErrorTrace(eptr) ->
		    Format.printf "@,The program is unsafe@,Error trace: %a@," TraceConstr.pr eptr
  in
  let _ = Global.log_end "verify" in
  ()

