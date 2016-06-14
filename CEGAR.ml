
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

type result = Safe of (var * CEGAR_ref_type.t) list | Unsafe of int list

module MC = ModelCheck

let pre () =
  ()

let post () =
  incr Flag.cegar_loop;
  Fpat.Global.cegar_iterations := !Flag.cegar_loop

let print_non_CPS_abst abst prog =
  if !Flag.just_print_non_CPS_abst then
    let result =
      try
        Some (MC.check abst prog MC.Other)
      with _ -> None
    in
    let s =
      match result with
      | None -> "Unknown"
      | Some (MC.Safe _) -> "Safe"
      | Some (MC.Unsafe _) -> "Unsafe"
    in
    Format.printf "@.ABST:@.%a@." CEGAR_print.prog abst;
    Format.printf "RESULT: %s@." s;
    exit 0

let improve_precision () =
  match () with
  | _ when not !Flag.use_filter ->
      if !Flag.print_progress then Format.printf "Filter option enabled.@.";
      if !Flag.print_progress then Format.printf "Restart CEGAR-loop.@.";
      Flag.use_filter := true
  | _ when not !Flag.never_use_neg_pred && not !Fpat.PredAbst.use_neg_pred ->
      if !Flag.print_progress then Format.printf "Negative-predicate option enabled.@.";
      if !Flag.print_progress then Format.printf "Restart CEGAR-loop.@.";
      Fpat.PredAbst.use_neg_pred := true
  | _ when !Fpat.PredAbst.wp_max_num < Flag.wp_max_max ->
      incr Fpat.PredAbst.wp_max_num;
      CEGAR_abst.incr_wp_max := true;
      if !Flag.print_progress then Format.printf "Set wp_max_num to %d.@." !Fpat.PredAbst.wp_max_num;
      if !Flag.print_progress then Format.printf "Restart CEGAR-loop.@.";
  | _ ->
      raise NoProgress

let rec loop prog0 is_cp ces info =
  pre ();
  let prog =
    if !Flag.relative_complete
    then
      let env,defs,main = FpatInterface.instantiate_param prog0 in
      {env; defs; main; info=init_info}
    else prog0
  in
  let pr =
    if !Flag.expand_nonrec
    then CEGAR_util.print_prog_typ' info.orig_fun_list info.inlined
    else CEGAR_print.prog_typ
  in
  if !Flag.print_progress
  then Format.printf "Program with abstraction types (CEGAR-cycle %d)::@.%a@." !Flag.cegar_loop pr prog;
  if !Flag.print_abst_typ
  then Format.printf "Abstraction types (CEGAR-cycle %d)::@.%a@." !Flag.cegar_loop CEGAR_print.env prog.env;
  let labeled,abst = CEGAR_abst.abstract info.orig_fun_list info.inlined prog in
  print_non_CPS_abst abst prog;
  let spec =
    match info.CEGAR_syntax.fairness with
    | Some x -> MC.Fairness x
    | None -> MC.Other in
  let result = MC.check abst prog spec in
  match result, !Flag.mode with
  | MC.Safe env, _ ->
      if Flag.print_ref_typ_debug
      then
        begin
          Format.printf "Intersection types:@.";
          List.iter (fun (f,typ) -> Format.printf "  %s: %a@." f Inter_type.print typ) env;
          Format.printf "@."
        end;
      let aux (x,ityp) =
        try
          Some (x, Type_trans.ref_of_inter (List.assoc x prog.env) ityp)
        with Not_found -> None
      in
      let env' = List.filter_map aux env in
      if Flag.print_ref_typ_debug
      then
        begin
          Format.printf "Refinement types:@.";
          List.iter (fun (f,typ) -> Format.printf "  %s: %a@." f CEGAR_ref_type.print typ) env';
          Format.printf "@."
        end;
      post ();
      prog, Safe env'
  | MC.Unsafe (MC.CENonTerm ce_tree), Flag.NonTermination ->
      let prog' = CEGAR_non_term.cegar prog0 labeled info is_cp ce_tree prog in
      post ();
      loop prog' is_cp ((MC.CENonTerm ce_tree)::ces) info
  | MC.Unsafe (MC.CEFairNonTerm ce_rules), Flag.FairNonTermination ->
     begin
       let prog' = CEGAR_fair_non_term.cegar prog0 labeled info is_cp ce_rules prog in
       post ();
       Fpat.PredAbst.use_neg_pred := true;
       let same_counterexample =
         match ces with
         | [] -> false
         | MC.CEFairNonTerm ce_pre :: _ -> ce_pre = ce_rules (*TODO*)
         | _ -> assert false
       in
       if same_counterexample then
         try
           improve_precision ();
           loop prog is_cp ces info
         with NoProgress ->
           post ();
           raise NoProgress
       else
         loop prog' is_cp ((MC.CEFairNonTerm ce_rules)::ces) info
     end
  | MC.Unsafe ce, _ ->
      let ce_orig =
        match ce with
        | MC.CESafety ce -> ce
        | _ -> assert false
      in
      if !Flag.print_eval_abst then CEGAR_trans.eval_abst_cbn prog labeled abst ce_orig;
      let ce' = CEGAR_trans.trans_ce labeled prog ce_orig in
      let same_counterexample =
        match ces with
        | [] -> false
        | MC.CESafety ce_pre :: _ -> ce' = CEGAR_trans.trans_ce labeled prog ce_pre
        | _ -> assert false
      in
      if same_counterexample then
        try
          improve_precision ();
          loop prog is_cp ces info
        with NoProgress ->
          post ();
          if !Flag.print_progress then Feasibility.print_ce_reduction ce' prog;
          raise NoProgress
      else
        begin
          if !Flag.print_progress then Feasibility.print_ce_reduction ce' prog;
          match Feasibility.check ce' prog, !Flag.mode with
          | Feasibility.Feasible sol, Flag.Termination ->
              (* termination analysis *)
              Refine.refine_rank_fun ce' [] prog0;
              assert false
          | Feasibility.Feasible sol, Flag.FairTermination ->
              Refine.refine_rank_fun ce' [] prog0;
              assert false
          | Feasibility.Feasible sol, _ ->
              prog, Unsafe sol
          | Feasibility.FeasibleNonTerm _, _ ->
              assert false
          | Feasibility.Infeasible prefix, _ ->
              let ces' = ce::ces in
              let inlined_functions = inlined_functions info.orig_fun_list info.inlined prog0 in
              let aux ce =
                match ce with
                | MC.CESafety ce' -> CEGAR_trans.trans_ce labeled prog ce'
                | _ -> assert false
              in
              let prog' =
                let ces'' = List.map aux ces' in
                let ext_ces = List.map (Fun.const []) ces'' in
                snd @@ Refine.refine inlined_functions is_cp prefix ces'' ext_ces prog0
              in
              if !Flag.debug_level > 0 then
                Format.printf "Prefix of spurious counterexample::@.%a@.@." CEGAR_print.ce prefix;
              post ();
              loop prog' is_cp ces' info
        end


let run prog info =
  if false then Format.printf "MAIN_LOOP: %a@." CEGAR_print.prog @@ Option.get prog.info.exparam_orig;
  let prog =
    match !Flag.mode with
    | Flag.NonTermination
    | Flag.FairNonTermination -> CEGAR_trans.add_fail_to_end prog
    | _ -> prog
  in
  make_ID_map prog;
  try
    let is_cp = FpatInterface.is_cp prog in
    snd @@ loop prog is_cp [] info
  with NoProgress | CEGAR_abst.NotRefined ->
    post ();
    raise NoProgress
