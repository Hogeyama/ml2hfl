
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

let rec loop prog0 is_cp ces info top_funs =
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
  let labeled,abst = CEGAR_abst.abstract info.orig_fun_list info.inlined prog in
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
      prog, Safe env'
  | ModelCheck.Unsafe ce ->
      if !Flag.print_eval_abst then CEGAR_trans.eval_abst_cbn prog labeled abst ce;
      let ce' = CEGAR_trans.trans_ce ce labeled prog in
      match ces with
      | ce_pre::_ when ce' = ce_pre && not !Flag.use_filter ->
          if !Flag.print_progress then Format.printf "Filter option enabled.@.";
          if !Flag.print_progress then Format.printf "Restart CEGAR-loop.@.";
          Flag.use_filter := true;
          loop prog is_cp ces info top_funs
      | ce_pre::_ when ce' = ce_pre && not !Flag.never_use_neg_pred && not !Fpat.PredAbst.use_neg_pred ->
          if !Flag.print_progress then Format.printf "Negative-predicate option enabled.@.";
          if !Flag.print_progress then Format.printf "Restart CEGAR-loop.@.";
          Fpat.PredAbst.use_neg_pred := true;
          loop prog is_cp ces info top_funs
      | ce_pre::_ when ce' = ce_pre && !Fpat.PredAbst.wp_max_num < Flag.wp_max_max ->
          incr Fpat.PredAbst.wp_max_num;
          CEGAR_abst.incr_wp_max := true;
          if !Flag.print_progress then Format.printf "Set wp_max_num to %d.@." !Fpat.PredAbst.wp_max_num;
          if !Flag.print_progress then Format.printf "Restart CEGAR-loop.@.";
          loop prog is_cp ces info top_funs
      | ce_pre::_ when ce' = ce_pre ->
          post ();
          if !Flag.print_progress then Feasibility.print_ce_reduction ce' prog;
          raise NoProgress
      | _ ->
          if !Flag.print_progress then Feasibility.print_ce_reduction ce' prog;
          match Feasibility.check ce' prog with
          | Feasibility.Feasible (env, sol) ->
              if !Flag.termination then
                begin
                  (* termination analysis *)
                  Refine.refine_rank_fun ce' prog0;
                  assert false
                end else
                prog, Unsafe sol
          | Feasibility.Infeasible prefix ->
              let ces' = ce'::ces in
              let inlined_functions = inlined_functions info.orig_fun_list info.inlined prog0 in
              let _,prog' = Refine.refine inlined_functions is_cp prefix ces' prog0 in
              if !Flag.debug_level > 0 then
                Format.printf "Prefix of spurious counterexample::@.%a@.@."
                              CEGAR_print.ce prefix;
              post ();
              loop prog' is_cp ces' info top_funs



let cegar prog info top_funs =
  try
    let is_cp = FpatInterface.is_cp prog in
    loop prog is_cp [] info top_funs
  with NoProgress | CEGAR_abst.NotRefined ->
    post ();
    raise NoProgress



let map1 =
  [1, fun x -> [make_gt x (make_int 0); make_lt x (make_int 0)]]

let map2 =
  let n = Var "n_1009" in
  [2, fun x -> [make_eq_int x (make_int 0); make_gt x n]]

let map3 =
  let n = Var "n_1009" in
  [2, fun x -> [make_or (make_gt x n) (make_leq x (make_sub (make_int 0) n))]]

(*
let cegar prog info =
  let x = new_id "x" in
  let prog' = {prog with env = add_renv map3 prog.env} in
  cegar prog' info
*)
