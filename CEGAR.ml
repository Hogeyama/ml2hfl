
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
      prog, Safe env'
  | ModelCheck.Unsafe (cexs, ext_cexs) ->
      let cexs' = List.map (fun ce -> CEGAR_trans.trans_ce ce labeled prog) cexs in
      let ext_cexs' = List.map (merge_ext_preds_sequence |- List.map (FpatInterface.trans_ext (get_renv prog) (map_randint_to_preds prog0.env))) ext_cexs in
      if !Flag.print_progress then List.iter (fun ce -> Feasibility.print_ce_reduction ce prog) cexs' ;
      let maps =
        List.map2
          (fun ce ext_ce ->
           match Feasibility.check ce prog with
           | Feasibility.Feasible (env, sol) ->
              let inlined_functions = inlined_functions info.orig_fun_list info.inlined prog0 in
              let map,_ = Refine.refine_with_ext inlined_functions is_cp [] [ce] [ext_ce] prog0 in
              map
           | Feasibility.Infeasible prefix ->
              let inlined_functions = inlined_functions info.orig_fun_list info.inlined prog0 in
              let map, p = Refine.refine inlined_functions is_cp prefix [ce] [ext_ce] prog0 in
              Format.printf "ENV: %a@." CEGAR_print.env p.env;
              if !Flag.debug_level > 0 then
                Format.printf "Prefix of spurious counterexample::@.%a@.@."
                              CEGAR_print.ce prefix;
              map)
          cexs' ext_cexs'
      in
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
