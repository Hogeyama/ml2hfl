open ExtList

let verify prog =
(*
  Format.printf "%a" Prog.pr prog;
*)
  try
		  let rt = Ctree.init_ctree_of prog in
    let strategy = if true then CtreeExpander.bf_strategy rt else CtreeExpander.df_strategy rt in
    let rec loop old_eps i =
      let _ = CtreeExpander.expand_until_new_error_path_found prog rt strategy in
				  let eps = Ctree.error_paths_of rt in
      let eps' = Util.diff eps old_eps in
      let _ =
        Format.printf "error paths:@.";
        List.iter (fun ep -> Format.printf "  %a@." Ctree.pr_path ep) eps'
      in
				  let eptrs = List.map Trace.of_error_path eps(*eps'*) in
      let fcs = List.unique (Util.concat_map Trace.function_calls_of eps) in
      let env = Trace.infer_env prog eptrs fcs in
      let _ = Format.printf "interaction types:@.  %a@." SizType.pr_env env in
      if SizType.check_prog env prog then
        Format.printf "@.The program is safe@."
      else
        loop eps (i + 1)
    in
    loop [] 1
  with Trace.FeasibleErrorTrace(eptr) ->
   Format.printf "@.The program is unsafe@.Error trace: %a@." Trace.pr eptr


let infer_abst_type cex prog =
(**)
  Format.printf "%a" Prog.pr prog;
(**)
  let _ = Format.printf "%s@." (String.concat ":" (List.map string_of_int cex)) in
  let rt = Ctree.init_ctree_of prog in
  let strategy = CtreeExpander.cex_strategy cex rt in
  let _ = CtreeExpander.expand_until_new_error_path_found prog rt strategy in
  let eps = Ctree.error_paths_of rt in
		let eptrs = List.map Trace.of_error_path eps in
  let fcs = List.unique (Util.concat_map Trace.function_calls_of eps) in
  let env = Trace.infer_env prog eptrs fcs in
(**)
  let _ = Format.printf "interaction types:@.  %a@." SizType.pr_env env in
(*
  let env = List.map (fun (f, sty) -> f, RefType.of_sized_type sty) env in
  let _ = Format.printf "refinement types:@.  %a@." RefType.pr_env env in
  let env = List.map (fun (f, sty) -> f, AbsType.of_refinement_type sty) env in  
*)
  let env = List.map (fun (f, sty) -> f, AbsType.of_sized_type f sty) env in
  let env =
    List.map
      (fun ((f, sty)::fstys) -> f, AbsType.merge (sty::List.map snd fstys))
      (Util.classify (fun (f1, _) (f2, _) -> f1 = f2) env)
  in
  let _ = Format.printf "abstraction types:@.  %a@." AbsType.pr_env env in
  env
