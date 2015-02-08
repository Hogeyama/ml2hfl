
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

exception NoProgress
exception CannotDiscoverPredicate

type result = Safe of (var * CEGAR_ref_type.t) list | Unsafe of int list
type info = {orig_fun_list:var list; inlined:var list}

let empty_info = {orig_fun_list=[]; inlined=[]}

let pre () =
  ()

let post () =
  incr Flag.cegar_loop;
  Fpat.Global.cegar_iterations := !Flag.cegar_loop





let inlined_functions orig_fun_list force {defs;main} =
  let fs = List.map fst (CEGAR_util.get_nonrec defs main orig_fun_list force) in
  FpatInterface.List.unique fs

let rec loop prog0 is_cp info top_funs =
  pre ();
  let prog =
    if !Flag.relative_complete
    then
      let env,defs,main = FpatInterface.instantiate_param (prog0.env,prog0.defs,prog0.main) in
      {env=env; defs=defs; main=main}
    else prog0
  in
  let pr =
    if !Flag.expand_nonrec
    then CEGAR_util.print_prog_typ' info.orig_fun_list info.inlined
    else CEGAR_print.prog_typ
  in
  if !Flag.print_progress
  then Format.printf "Program with abstraction types (CEGAR-cycle %d)::@.%a@." !Flag.cegar_loop pr prog;
  let labeled,abst = CEGAR_abst.abstract info.orig_fun_list info.inlined prog ~top_funs:top_funs in
  let result = ModelCheck.check abst prog top_funs in
  match result with
  | ModelCheck.Safe env ->
      if Flag.print_ref_typ_debug
      then
        begin
          Format.printf "Intersection types:@.";
          List.iter (fun (f,typ) -> Format.printf "  %s: %a@." f Inter_type.print typ) env;
          Format.printf "@."
        end;
      let aux (x,ityp) =
        try
          [x, Type_trans.ref_of_inter (List.assoc x prog.env) ityp]
        with Not_found -> []
      in
      let env' = List.rev_map_flatten aux env in
      post ();
      let log_file =
	if !Flag.randint_refinement_log then 
	  let dirname = Filename.dirname !Flag.filename in
	  let basename = Filename.basename !Flag.filename in
	  dirname ^ "/refinement/" ^
	    (try Filename.chop_extension basename ^ ".refinement"
	     with Invalid_argument "Filename.chop_extension" -> basename ^ ".refinement")
	else ""
      in
      let log_cout = if !Flag.randint_refinement_log then open_out_gen [Open_wronly; Open_append; Open_text; Open_creat] 0o666 log_file else stdout in
      let log_fm = Format.formatter_of_out_channel log_cout in
      (* Format.printf "ABSTRACTION TYPE ENV:@.%a@." CEGAR_print.env_diff prog.env; *)
      if !Flag.randint_refinement_log then Format.fprintf log_fm "---------------[Safe]---------------@.ABSTRACTION TYPE ENV:@.%a@." CEGAR_print.env_diff prog.env;
      if !Flag.randint_refinement_log then close_out log_cout;
      prog, Safe env'
  | ModelCheck.Unsafe (cexs, ext_cexs) ->
      let map_randint_to_preds = make_map_randint_to_preds prog0 in
      let renv = get_renv prog in
      let path_counter = ref 0 in

      let print_path fm p = Format.fprintf fm "%a" (print_list Format.pp_print_string ",") (List.map (fun n -> if n=0 then "then" else "else") p) in
      let print_ext_path_parts fm p = Format.fprintf fm "%s"
	(match p with
	  | Positive -> "true"
	  | Negative -> "false"
	  | Do_not_Care -> "_") in
      let print_ext_path fm p = Format.fprintf fm "[%a]" (print_list print_ext_path_parts ",") p in
      let print_ext_paths fm (n, p) = Format.fprintf fm "%d:%a" n (print_list print_ext_path ",") p in
      let print_pred ppf (env, fml) =
	Format.fprintf ppf "%a |= %a" Fpat.TypEnv.pr_compact env Fpat.Formula.pr fml in
      let log_file =
	if !Flag.randint_refinement_log then 
	  let dirname = Filename.dirname !Flag.filename in
	  let basename = Filename.basename !Flag.filename in
	  dirname ^ "/refinement/" ^
	    (try Filename.chop_extension basename ^ ".refinement"
	     with Invalid_argument "Filename.chop_extension" -> basename ^ ".refinement")
	else ""
      in

      let debug = !Flag.debug_level > 0 in
      if debug then Format.printf "@.ABSTRACTION TYPE ENV:@.%a@." CEGAR_print.env_diff prog.env;

      let paths =
        List.filter_map2
          (fun orig_ce ext_ce ->
	    path_counter := !path_counter + 1;
	    let ce = CEGAR_trans.trans_ce orig_ce labeled prog in
	    if !Flag.print_progress then Feasibility.print_ce_reduction ~map_randint_to_preds ~ext_ce ce prog;
	    let ext_path = ext_ce |> arrange_ext_preds_sequence |> conv_path in
	    (* let ext_preds = ext_path |> List.map (FpatInterface.trans_ext renv map_randint_to_preds) in *)
	    let path = Feasibility.check ~map_randint_to_preds ~ext_ce ce prog in
	    match path with
	      | Feasibility.Feasible (true, env, sol) ->
		if debug then Format.printf "[%d: path %d] Found useless feasible path: [%a]@." !Flag.cegar_loop !path_counter print_path orig_ce;
		None (* do not use a useless (i.e. already-used-in-CEGAR) error-path *)
	      | Feasibility.Feasible (false, env, sol) ->
		if debug then Format.printf "[%d: path %d] Found feasible path: [%a]@." !Flag.cegar_loop !path_counter print_path orig_ce;
	        Some (path, orig_ce, ce, ext_path)
	      | Feasibility.Infeasible prefix ->
		if debug then Format.printf "[%d: path %d] Found infeasible path: [%a]@." !Flag.cegar_loop !path_counter print_path orig_ce;
	        Some (path, orig_ce, ce, ext_path))
          cexs ext_cexs
      in

      (* Progress Check: checking whether an obtained counterexample tree has either infeasible paths or open feasible paths *)
      if paths = [] then raise NoProgress;

      if debug then Format.printf "ORIG:@.";
      if debug then List.iter (fun (_, orig_ce, _, ext_path) -> Format.printf "%a: @.  %a@." print_path orig_ce (print_list print_ext_paths ";") ext_path) paths;

      let paths = List.concat @@ List.map (if !Flag.merge_paths_of_same_branch then merge_similar_paths else (fun x -> x)) (group_by_same_branching paths) in

      if !Flag.merge_paths_of_same_branch then
	begin
	  if debug then Format.printf "MERGED:@.";
	  if debug then List.iter (fun (_, orig_ce, _, ext_path) -> Format.printf "%a: @.  %a@." print_path orig_ce (print_list print_ext_paths ";") ext_path) paths;
	end;
      
      path_counter := 0;
      let refinement_type_map = function
	| (Feasibility.Feasible (true, env, sol), _, _, _) ->
	  assert false
	| (Feasibility.Feasible (false, env, sol), orig_ce, ce, ext_path) ->
	  let log_cout = if !Flag.randint_refinement_log then open_out_gen [Open_wronly; Open_append; Open_text; Open_creat] 0o666 log_file else stdout in
	  let log_fm = Format.formatter_of_out_channel log_cout in

	  if debug then Format.printf "[%d: path %d] Refining by feasible path: [%a]@." !Flag.cegar_loop !path_counter print_path orig_ce;
	  if !Flag.randint_refinement_log then Format.fprintf log_fm "[%d: path %d] Refining by feasible path: [%a]@." !Flag.cegar_loop !path_counter print_path orig_ce;
	  let ext_preds = ext_path |> List.map (FpatInterface.trans_ext renv map_randint_to_preds) in
	  if debug then List.iter (fun (rand_var, preds_sequence) -> Format.printf "%a: %a@." Fpat.Idnt.pr rand_var (print_list print_pred " , ") preds_sequence) ext_preds;
	  if !Flag.randint_refinement_log then List.iter (fun (rand_var, preds_sequence) -> Format.fprintf log_fm "%a: %a@." Fpat.Idnt.pr rand_var (print_list print_pred " , ") preds_sequence) ext_preds;
	  let inlined_functions = inlined_functions info.orig_fun_list info.inlined prog0 in
	  let map,_ = Refine.refine_with_ext inlined_functions is_cp [] [ce] [ext_preds] prog0 in
	  let map = CEGAR_trans.add_neg_preds_renv map in
	  if debug then Format.printf "REFINEMENT MAP:@.%a@." CEGAR_print.env_diff map;
	  if !Flag.randint_refinement_log then Format.fprintf log_fm "REFINEMENT MAP:@.%a@." CEGAR_print.env_diff map;
	  if !Flag.randint_refinement_log then close_out log_cout;
	  map
	| (Feasibility.Infeasible prefix, orig_ce, ce, ext_path) ->
	  let log_cout = if !Flag.randint_refinement_log then open_out_gen [Open_wronly; Open_append; Open_text; Open_creat] 0o666 log_file else stdout in
	  let log_fm = Format.formatter_of_out_channel log_cout in

	  if debug then Format.printf "[%d: path %d] Refining by infeasible path: [%a]@." !Flag.cegar_loop !path_counter print_path orig_ce;
	  if !Flag.randint_refinement_log then Format.fprintf log_fm "[%d: path %d] Refining by infeasible path: [%a]@." !Flag.cegar_loop !path_counter print_path orig_ce;
	  let ext_preds = ext_path |> List.map (FpatInterface.trans_ext renv map_randint_to_preds) in
	  if debug then List.iter (fun (rand_var, preds_sequence) -> Format.printf "%a: %a@." Fpat.Idnt.pr rand_var (print_list print_pred " , ") preds_sequence) ext_preds;
	  if !Flag.randint_refinement_log then List.iter (fun (rand_var, preds_sequence) -> Format.fprintf log_fm "%a: %a@." Fpat.Idnt.pr rand_var (print_list print_pred " , ") preds_sequence) ext_preds;
	  let inlined_functions = inlined_functions info.orig_fun_list info.inlined prog0 in
	  let map, p = Refine.refine inlined_functions is_cp prefix [ce] [ext_preds] prog0 in
	  if debug then Format.printf "REFINEMENT MAP:@.%a@." CEGAR_print.env_diff map;
	  if !Flag.randint_refinement_log then Format.fprintf log_fm "REFINEMENT MAP:@.%a@." CEGAR_print.env_diff map;
	  if !Flag.randint_refinement_log then close_out log_cout;
	  map
      in
      let maps = List.map (fun path -> path_counter := !path_counter + 1; refinement_type_map path) paths in
      let env' = List.fold_left (fun a b -> Refine.add_preds_env b a) prog.env maps in
      post ();
      loop {prog with env=env'} is_cp info top_funs



let cegar prog info top_funs =
  let add_fail_to_end ds =
    if !Flag.non_termination then
      List.map (fun (f, args, cond, e, t) -> if t=Const(CPS_result) then (f, args, cond, [Event "fail"], t) else (f, args, cond, e, t)) ds
    else ds in
  let prog = {prog with defs=add_fail_to_end prog.defs} in
  try
    let is_cp = FpatInterface.is_cp prog in
    loop prog is_cp info top_funs
  with NoProgress | CEGAR_abst.NotRefined ->
    post ();
    raise NoProgress

(*

let map1 =
  [1, fun x -> [make_gt x (make_int 0); make_lt x (make_int 0)]]

let map2 =
  let n = Var "n_1009" in
  [2, fun x -> [make_eq_int x (make_int 0); make_gt x n]]

let map3 =
  let n = Var "n_1009" in
  [2, fun x -> [make_and (make_gt x n) (make_leq x (make_sub (make_int 0) n))]]

let map4 = [1, fun x -> [make_lt x (make_int (-1))]]

let cegar prog info top_funs =
  let x = new_id "x" in
  let prog' = {prog with env = Refine.add_renv ([](*map3@map4*)) prog.env} in
  cegar prog' info top_funs
*)
