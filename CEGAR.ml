
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

exception NoProgress
exception CannotDiscoverPredicate

type info = {orig_fun_list:var list; inlined:var list}

let make_ce_printer ce prog sol () =
  Format.printf "Inputs:@.";
  List.iter (fun t -> Format.printf "  %s;@." t) sol;
  Feasibility.print_ce_reduction ce prog

let pre () =
  Id.save_counter ()

let post () =
  incr Flag.cegar_loop;
  (**)
  Wrapper.reopen_cvc3 ()(*;
  (**)
  Id.reset_counter ()
*)

let inlined_functions orig_fun_list force {defs=defs;main=main} =
  let fs = List.map fst (CEGAR_util.get_nonrec defs main orig_fun_list force) in
  ExtList.List.unique fs
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
      let env,defs,main = LazyInterface.instantiate_param (prog0.env,prog0.defs,prog0.main) in
        {env=env; defs=defs; main=main}
    else prog0
  in
  let () = Format.printf "Program with abstraction types (CEGAR-cycle %d)::@.%a@." !Flag.cegar_loop pr prog in
  let labeled,abst = CEGAR_abst.abstract info.orig_fun_list info.inlined None prog in
  let result = ModelCheck.check None abst prog in
  let result' = apply_opt (fun ce -> CEGAR_trans.trans_ce ce labeled prog) result in
    match result',ces with
        None,_ -> prog, None
      | Some ce, ce'::_ when ce = ce' && not !Flag.use_filter ->
          Format.printf "Filter option enabled.@.";
          Format.printf "Restart CEGAR-loop.@.";
          Flag.use_filter := true;
          cegar1 prog ces info
      | Some ce, ce'::_ when ce = ce' && not !Flag.never_use_neg_pred && not !Flag.use_neg_pred ->
          Format.printf "Negative-predicate option enabled.@.";
          Format.printf "Restart CEGAR-loop.@.";
          Flag.use_neg_pred := true;
          cegar1 prog ces info
      | Some ce, ce'::_ when ce = ce' && !Flag.wp_max_num < 8 ->
          Flag.wp_max_num := !Flag.wp_max_num + 1;
          Format.printf "Set wp_max_num to %d.@." !Flag.wp_max_num;
          Format.printf "Restart CEGAR-loop.@.";
          cegar1 prog ces info
      | Some ce, ce'::_ when ce = ce' ->
          let ce_labeled = get_opt_val result in
          Feasibility.print_ce_reduction ce prog;
          if !Flag.print_eval_abst then CEGAR_trans.eval_abst_cbn prog labeled abst ce_labeled;
          raise NoProgress
      | Some ce, _ ->
          let ce_labeled = get_opt_val result in
          Feasibility.print_ce_reduction ce prog;
          if !Flag.print_eval_abst then CEGAR_trans.eval_abst_cbn prog labeled abst ce_labeled;
          match Feasibility.check ce prog with
              Feasibility.Feasible (env, sol) -> prog, Some (make_ce_printer ce prog sol)
            | Feasibility.Infeasible prefix ->
                let () =
                  if true
                  then
                    Format.printf "Prefix of spurious counter-example::@.%a@.@."
                      CEGAR_print.ce prefix
                in
                let ces' = ce::ces in
                let inlined_functions = inlined_functions info.orig_fun_list info.inlined prog0 in
                let _,prog' = Refine.refine inlined_functions prefix ces' prog0 in
(*                let prog' = reconstruct_typ prog' in*)
                  post ();
    cegar1 prog' ces' info



let cegar prog orig_fun_list = cegar1 prog [] orig_fun_list
