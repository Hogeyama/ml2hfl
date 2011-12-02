open ExtList

let refineRefTypes prog etrs =
		let constrss = List.map CgenRefType.cgen etrs in
(**)
  let _ = Format.printf "constraints:@.  @[<v>%a@]@." (Util.pr_list Cgen.pr "@,") constrss in
(**)
  let sums = CgenRefType.summaries_of (Prog.type_of prog) constrss in
(*
  let _ = List.iter (function `Pre((x, uid), pre) ->
    Format.printf "Pre(%a,%d): %a@." Var.pr x uid Term.pr pre
  | `Post((x, uid), post) ->
    Format.printf "Post(%a,%d): %a@." Var.pr x uid Term.pr post) sums
  in
*)
  let fcs = List.unique (Util.concat_map CompTree.function_calls_of etrs) in
  let env = CgenRefType.infer_env prog sums fcs in
  env

let refineIntTypes prog etrs =
		let constrss = List.map CgenIntType.cgen etrs in
(**)
  let _ = Format.printf "constraints:@.  @[<v>%a@]@." (Util.pr_list Cgen.pr "@,") constrss in
(**)
  let sums = Util.concat_map
    (fun constrs ->
      Format.printf "@.";
      CgenIntType.summaries_of constrs)
    constrss
  in
(*
  let _ = List.iter (function `Pre((x, uid), pre) ->
    Format.printf "Pre(%a,%d): %a@." Var.pr x uid Term.pr pre
  | `Post((x, uid), post) ->
    Format.printf "Post(%a,%d): %a@." Var.pr x uid Term.pr post) sums
  in
*)
  let fcs = List.unique (Util.concat_map CompTree.function_calls_of etrs) in
  let env = CgenIntType.infer_env prog sums fcs in
  env

let verify prog =
(*
  Format.printf "%a" Prog.pr prog;
*)
  try
		  let rt = CompTree.init_ctree_of prog in
    let strategy = if true then CompTreeExpander.bf_strategy rt else CompTreeExpander.df_strategy rt in
    let rec loop old_etrs i =
      let _ = CompTreeExpander.expand_until_new_error_trace_found prog rt strategy in
				  let etrs = CompTree.error_traces_of rt in
      let etrs' = Util.diff etrs old_etrs in
      let _ =
        Format.printf "error traces:@.";
        List.iter (fun ep -> Format.printf "  %a@." CompTree.pr_trace ep) etrs'
      in
      match Flags.refine with `RefType ->
		      let env = refineRefTypes prog etrs(*etrs'*) in
		      if IntType.check_prog env prog then
		        Format.printf "@.The program is safe@."
		      else
		        loop etrs (i + 1)
      | `IntType ->
		      let env = refineIntTypes prog etrs(*etrs'*) in
		      if IntType.check_prog env prog then
		        Format.printf "@.The program is safe@."
		      else
		        loop etrs (i + 1)
    in
    loop [] 1
  with Cgen.FeasibleErrorTrace(eptr) ->
   Format.printf "@.The program is unsafe@.Error trace: %a@." Cgen.pr eptr


let infer_abst_type cex prog =
(*
  let _ = Format.printf "%a" Prog.pr prog in
*)
  let _ = Format.printf "%s@." (String.concat ":" (List.map string_of_int cex)) in
  let rt = CompTree.init_ctree_of prog in
  let strategy = CompTreeExpander.cex_strategy cex rt in
  let _ = CompTreeExpander.expand_until_new_error_trace_found prog rt strategy in
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
		    (fun ((f, sty)::fstys) -> f, AbsType.merge (sty::List.map snd fstys))
		    (Util.classify (fun (f1, _) (f2, _) -> f1 = f2) env)
		in
		let _ = Format.printf "abstraction types:@.  %a@." AbsType.pr_env env in
		env
