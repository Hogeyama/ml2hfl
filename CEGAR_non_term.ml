open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

let cegar prog0 labeled info is_cp ce prog =
  let (cexs, ext_cexs) =
    match ce with
    | ModelCheck.CEHorSat(cexs, ext_cexs) -> cexs, ext_cexs
    | _ -> assert false
  in
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
      dirname ^ "/refinement/" ^ Filename.change_extension basename "refinement"
    else ""
  in

  let debug = !Flag.debug_level > 0 in
  if debug then Format.printf "@.ABSTRACTION TYPE ENV:@.%a@." CEGAR_print.env_diff prog.env;

  let paths =
    List.filter_map2
      (fun orig_ce ext_ce ->
        path_counter := !path_counter + 1;
        let ce = CEGAR_trans.trans_ce labeled prog orig_ce in
        if !Flag.print_progress then Feasibility.print_ce_reduction ~map_randint_to_preds ~ext_ce ce prog;
        let ext_path = ext_ce |> arrange_ext_preds_sequence |> conv_path in
        (* let ext_preds = ext_path |> List.map (FpatInterface.trans_ext renv map_randint_to_preds) in *)
        let path = Feasibility.check_non_term ~map_randint_to_preds ~ext_ce ce prog in
        match path with
          | Feasibility.Feasible _ -> assert false
          | Feasibility.FeasibleNonTerm (true, env, sol) ->
            if debug then Format.printf "[%d: path %d] Found useless feasible path: [%a]@." !Flag.cegar_loop !path_counter print_path orig_ce;
            None (* do not use a useless (i.e. already-used-in-CEGAR) error-path *)
          | Feasibility.FeasibleNonTerm (false, env, sol) ->
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
    | (Feasibility.Feasible _, _, _, _) -> assert false
    | (Feasibility.FeasibleNonTerm (true, env, sol), _, _, _) ->
        assert false
    | (Feasibility.FeasibleNonTerm (false, env, sol), orig_ce, ce, ext_path) ->
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
  {prog with env=env'}
