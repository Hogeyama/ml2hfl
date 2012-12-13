
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open VHorn.ExtList

exception NoProgress
exception CannotDiscoverPredicate

type result = Safe of (var * CEGAR_ref_type.t) list | Unsafe of int list
type info = {orig_fun_list:var list; inlined:var list}

let pre () =
  Id.save_counter ()

let post () =
  incr Flag.cegar_loop;
  (**)
  Wrapper.reopen_cvc3 ();
  (**)
  Id.reset_counter ()


let inlined_functions orig_fun_list force {defs=defs;main=main} =
  let fs = List.map fst (CEGAR_util.get_nonrec defs main orig_fun_list force) in
  List.unique fs
(*Util.diff (List.map (fun (f, _, _, _, _) -> f) defs) *)

let rec cegar1 prog0 ces info =
  pre ();
  let pr =
    if !Flag.expand_nonrec
    then CEGAR_util.print_prog_typ' info.orig_fun_list info.inlined
    else CEGAR_print.prog_typ
  in
  let prog =
    if (match !Flag.refine with Flag.RefineRefType(_) -> true | _ -> false) && !Flag.relative_complete
    then
      let env,defs,main = VhornInterface.instantiate_param (prog0.env,prog0.defs,prog0.main) in
        {env=env; defs=defs; main=main}
    else prog0
  in
  let () =
    if !Flag.print_progress
    then Format.printf "Program with abstraction types (CEGAR-cycle %d)::@.%a@." !Flag.cegar_loop pr prog
  in
  let labeled,abst = CEGAR_abst.abstract info.orig_fun_list info.inlined prog in
  let result = ModelCheck.check abst prog in
    match result with
        ModelCheck.Safe env ->
          let aux (x,ityp) =
            try
              [x, Type_trans.ref_of_inter (List.assoc x prog.env) ityp]
            with Not_found -> []
          in
          let env' = rev_map_flatten aux env in
            prog, Safe env'
      | ModelCheck.Unsafe ce ->
          if !Flag.print_eval_abst then CEGAR_trans.eval_abst_cbn prog labeled abst ce;
          let ce' = CEGAR_trans.trans_ce ce labeled prog in
            match ces with
                ce_pre::_ when ce' = ce_pre && not !Flag.use_filter ->
                  if !Flag.print_progress then Format.printf "Filter option enabled.@.";
                  if !Flag.print_progress then Format.printf "Restart CEGAR-loop.@.";
                  Flag.use_filter := true;
                  cegar1 prog ces info
              | ce_pre::_ when ce' = ce_pre && not !Flag.never_use_neg_pred && not !Flag.use_neg_pred ->
                  if !Flag.print_progress then Format.printf "Negative-predicate option enabled.@.";
                  if !Flag.print_progress then Format.printf "Restart CEGAR-loop.@.";
                  Flag.use_neg_pred := true;
                  cegar1 prog ces info
              | ce_pre::_ when ce' = ce_pre && !Flag.wp_max_num < 8 ->
                  incr Flag.wp_max_num;
                  if !Flag.print_progress then Format.printf "Set wp_max_num to %d.@." !Flag.wp_max_num;
                  if !Flag.print_progress then Format.printf "Restart CEGAR-loop.@.";
                  cegar1 prog ces info
              | ce_pre::_ when ce' = ce_pre ->
                  if !Flag.print_progress then Feasibility.print_ce_reduction ce' prog;
                  raise NoProgress
              | _ ->
                  if !Flag.print_progress then Feasibility.print_ce_reduction ce' prog;
                  match Feasibility.check ce' prog with
                      Feasibility.Feasible (env, sol) -> prog, Unsafe sol
                    | Feasibility.Infeasible prefix ->
                        let ces' = ce'::ces in
                        let inlined_functions = inlined_functions info.orig_fun_list info.inlined prog0 in
                        let _,prog' = Refine.refine inlined_functions prefix ces' prog0 in
                          if !Flag.debug_level > 0 then
                            Format.printf "Prefix of spurious counter-example::@.%a@.@."
                              CEGAR_print.ce prefix;
                          post ();
                          cegar1 prog' ces' info



let cegar prog orig_fun_list = cegar1 prog [] orig_fun_list
