open ExtList

(** Verifier *)

let refineRefTypes prog etrs =
  let sums =
		  if true then
						let ctrs, hcss = List.split (List.map (HcGenRefType.cgen (Prog.type_of prog)) etrs) in
		    let hcs = List.concat hcss in
				  let _ = Format.printf "call trees:@.  @[<v>%a@]@." (Util.pr_list CallTree.pr "@,") ctrs in
				  let _ = Format.printf "horn clauses:@.  @[<v>%a@]@." (Util.pr_list HornClause.pr "@,") hcs in
      let ts = HornClause.formula_of hcs in
      let _ = Format.printf "verification conditions:@.  @[<v>%a@]@." (Util.pr_list Term.pr "@,") ts in
      let t = Farkas.farkas (Formula.band ts) in
      let _ = Format.printf "constraint on coefficients:@.  @[%a@]@." Term.pr t in
      let qft = AtpInterface.real_qelim t in
      let _ = Format.printf "solution of the constraint:@.  @[%a@]@." Term.pr qft in
		    let sol = HcSolve.solve ctrs hcs in
      List.map (fun ((x, _), t) -> `P(x, t)) sol
		  else
						let constrss = List.map TcGenRefType.cgen etrs in
				  (*
				  let _ = Format.printf "constraints:@.  @[<v>%a@]@." (Util.pr_list TraceConstr.pr "@,") constrss in
				  *)
				  TcSolveRefType.summaries_of (Prog.type_of prog) constrss
  in
  (**)
		let _ = List.iter (fun (`P(x, t)) ->
		  Format.printf "P[%a]: %a@." Var.pr x Term.pr t) sums
		in
		(**)
		let fcs = List.unique (Util.concat_map Trace.function_calls_of etrs) in
		let env = TcSolveRefType.infer_env prog sums fcs in
		env

let refineIntTypes prog etrs =
		let constrss = List.map TcGenIntType.cgen etrs in
(*
  let _ = Format.printf "constraints:@.  @[<v>%a@]@." (Util.pr_list TraceConstr.pr "@,") constrss in
*)
  let sums = Util.concat_map
    (fun constrs ->
      Format.printf "@.";
      TcSolveIntType.summaries_of (Prog.type_of prog) constrs)
    constrss
  in
(*
  let _ = List.iter (function `Pre((x, uid), pre) ->
    Format.printf "Pre(%a,%d): %a@." Var.pr x uid Term.pr pre
  | `Post((x, uid), post) ->
    Format.printf "Post(%a,%d): %a@." Var.pr x uid Term.pr post) sums
  in
*)
  let fcs = List.unique (Util.concat_map Trace.function_calls_of etrs) in
  let env = TcSolveIntType.infer_env prog sums fcs in
  env

let verify prog =
  let _ = if !Flags.debug then Format.printf "%a" Prog.pr prog in
  try
		  let rt = CompTree.init prog in
    let strategy = if true then CompTreeExpander.bf_strategy rt else CompTreeExpander.df_strategy rt in
    let rec loop old_etrs i =
      let _ = CompTreeExpander.expand_until_new_error_trace_found prog rt strategy in
				  let etrs = CompTree.error_traces_of rt in
      let etrs' = Util.diff etrs old_etrs in
      let _ =
        Format.printf "error traces:@.";
        List.iter (fun ep -> Format.printf "  %a@." Trace.pr ep) etrs'
      in
      match Flags.refine with
        `RefType ->
				      let env = refineRefTypes prog etrs(*etrs'*) in
  						  let _ = Format.printf "refinement types:@.  %a@." RefType.pr_env env in
				      if RefTypeCheck.check_prog env prog then
				        Format.printf "@.The program is safe@."
				      else
				        loop etrs (i + 1)
      | `IntType ->
				      let env = refineIntTypes prog etrs(*etrs'*) in
          let _ = Format.printf "interaction types:@.  %a@." IntType.pr_env env in
				      if IntTypeCheck.check_prog env prog then
				        Format.printf "@.The program is safe@."
				      else
				        loop etrs (i + 1)
    in
    loop [] 1
  with TraceConstr.FeasibleErrorTrace(eptr) ->
   Format.printf "@.The program is unsafe@.Error trace: %a@." TraceConstr.pr eptr


let infer_abst_type cexs prog =
(*
  let _ = Format.printf "%a" Prog.pr prog in
*)
  let _ = List.iter (fun cex -> Format.printf "%s@." (String.concat ":" (List.map string_of_int cex))) cexs in
  let rt = CompTree.init prog in
  let filter cts = List.filter (fun ct -> List.exists (Util.is_prefix ct.CompTree.path) cexs) cts in
  let strategy = CompTreeExpander.filter_strategy filter rt in
  let _ = CompTreeExpander.expand_all(*expand_until_new_error_trace_found*) prog rt strategy in
  let etrs = CompTree.error_traces_of rt in
  let env =
		  match Flags.refine with
		    `RefType ->
						  let env = refineRefTypes prog etrs in
						  let _ = Format.printf "refinement types:@.  %a@." RefType.pr_env env in
						  List.map (fun (f, sty) -> f, AbsType.of_refinement_type sty) env
		  | `IntType ->
						  let env = refineIntTypes prog etrs in
        let _ = Format.printf "interaction types:@.  %a@." IntType.pr_env env in
						  let env = List.map (fun (f, sty) -> f, RefType.of_interaction_type sty) env in
						  let _ = Format.printf "refinement types:@.  %a@." RefType.pr_env env in
						  List.map (fun (f, sty) -> f, AbsType.of_refinement_type sty) env
        (*
						  List.map (fun (f, sty) -> f, AbsType.of_interaction_type f sty) env
  						*)
  in
		let env =
		  List.map
		    (function ((f, sty)::fstys) -> f, AbsType.merge (sty::List.map snd fstys) | _ -> assert false)
		    (Util.classify (fun (f1, _) (f2, _) -> f1 = f2) env)
		in
		let _ = Format.printf "abstraction types:@.  %a@." AbsType.pr_env env in
		env
