
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
        Some (MC.check abst prog Other)
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

let rec loop prog0 is_cp ces info =
  pre ();
  let prog =
    if !Flag.relative_complete
    then
      let env,defs,main = FpatInterface.instantiate_param (prog0.env,prog0.defs,prog0.main) in
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
          [x, Type_trans.ref_of_inter (List.assoc x prog.env) ityp]
        with Not_found -> []
      in
      let env' = List.rev_map_flatten aux env in
      post ();
      prog, Safe env'
  | MC.Unsafe ce, Flag.NonTermination ->
      let prog' = CEGAR_non_term.cegar prog0 labeled info is_cp ce prog in
      post ();
      loop prog' is_cp (ce::ces) info
  | MC.Unsafe ce, _ ->
      let ce_orig =
        match ce with
        | MC.CESafety ce -> ce
        | _ -> assert false
      in
      if !Flag.print_eval_abst then CEGAR_trans.eval_abst_cbn prog labeled abst ce_orig;
      let ce' = CEGAR_trans.trans_ce labeled prog ce_orig in
      let ce_pre =
        match ces with
        | [] -> None
        | MC.CESafety ce_pre :: _ -> Some (CEGAR_trans.trans_ce labeled prog ce_pre)
        | _ -> assert false
      in
      if Some ce' = ce_pre then
        if not !Flag.use_filter then
          begin
            if !Flag.print_progress then Format.printf "Filter option enabled.@.";
            if !Flag.print_progress then Format.printf "Restart CEGAR-loop.@.";
            Flag.use_filter := true;
            loop prog is_cp ces info
          end
        else if not !Flag.never_use_neg_pred && not !Fpat.PredAbst.use_neg_pred then
          begin
            if !Flag.print_progress then Format.printf "Negative-predicate option enabled.@.";
            if !Flag.print_progress then Format.printf "Restart CEGAR-loop.@.";
            Fpat.PredAbst.use_neg_pred := true;
            loop prog is_cp ces info
          end
        else if !Fpat.PredAbst.wp_max_num < Flag.wp_max_max then
          begin
            incr Fpat.PredAbst.wp_max_num;
            CEGAR_abst.incr_wp_max := true;
            if !Flag.print_progress then Format.printf "Set wp_max_num to %d.@." !Fpat.PredAbst.wp_max_num;
            if !Flag.print_progress then Format.printf "Restart CEGAR-loop.@.";
            loop prog is_cp ces info
          end
        else
          begin
            post ();
            if !Flag.print_progress then Feasibility.print_ce_reduction ce' prog;
            raise NoProgress
          end
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
              let ces'' = List.map aux ces' in
              let ext_ces = List.make (List.length ces'') [] in
              let _,prog' = Refine.refine inlined_functions is_cp prefix ces'' ext_ces prog0 in
              if !Flag.debug_level > 0 then
                Format.printf "Prefix of spurious counterexample::@.%a@.@."
                              CEGAR_print.ce prefix;
              post ();
              loop prog' is_cp ces' info
        end



let run prog info =
  if false then Format.printf "MAIN_LOOP: %a@." CEGAR_print.prog @@ Option.get prog.info.exparam_orig;
  let add_fail_to_end ds =
    match !Flag.mode with
    | Flag.NonTermination ->
        List.map (fun (f, args, cond, e, t) -> if t=Const(CPS_result) then (f, args, cond, [Event "fail"], t) else (f, args, cond, e, t)) ds
    | _ -> ds
  in
  let prog = {prog with defs=add_fail_to_end prog.defs} in
  make_ID_map prog;
  try
    let is_cp = FpatInterface.is_cp prog in
    loop prog is_cp [] info
  with NoProgress | CEGAR_abst.NotRefined ->
    post ();
    raise NoProgress
