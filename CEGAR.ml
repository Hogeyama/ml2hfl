
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

exception NoProgress
exception CannotDiscoverPredicate


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

let inlined_functions orig_fun_list prog =
  let _, defs, main = prog in
  let fs = List.map fst (CEGAR_util.get_nonrec defs main orig_fun_list) in
  ExtList.List.unique fs
(*Util.diff (List.map (fun (f, _, _, _, _) -> f) defs) *)

let rec cegar1 prog0 ces orig_fun_list =
  pre ();
  let pr =
    if !Flag.expand_nonrec
    then CEGAR_util.print_prog_typ' orig_fun_list
    else CEGAR_print.prog_typ
  in
  let prog =
    if (match !Flag.refine with Flag.RefineRefType(_) -> true | _ -> false) && !Flag.relative_complete
    then LazyInterface.instantiate_param prog0
    else prog0
  in
  let () = Format.printf "Program with abstraction types (CEGAR-cycle %d)::@.%a@." !Flag.cegar_loop pr prog in
  let labeled,abst = CEGAR_abst.abstract orig_fun_list None prog in
  let result = ModelCheck.check None abst prog in
  let result' = apply_opt (fun ce -> CEGAR_trans.trans_ce ce labeled prog) result in
    match result',ces with
        None,_ -> prog, None
      | Some ce, ce'::_ when ce = ce' && not !Flag.use_filter ->
          Format.printf "Filter option enabled.@.";
          Format.printf "Restart CEGAR-loop.@.";
          Flag.use_filter := true;
          cegar1 prog ces orig_fun_list
      | Some ce, ce'::_ when ce = ce' && not !Flag.use_neg_pred ->
          Format.printf "Negative-predicate option enabled.@.";
          Format.printf "Restart CEGAR-loop.@.";
          Flag.use_neg_pred := true;
          cegar1 prog ces orig_fun_list
      | Some ce, ce'::_ when ce = ce' && !Flag.wp_max_num < 8 ->
          Flag.wp_max_num := !Flag.wp_max_num + 1;
          Format.printf "Set wp_max_num to %d.@." !Flag.wp_max_num;
          Format.printf "Restart CEGAR-loop.@.";
          cegar1 prog ces orig_fun_list
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
                let _,prog' = Refine.refine (inlined_functions orig_fun_list prog0) prefix ces' prog0 in
(*                let prog' = reconstruct_typ prog' in*)
                  post ();
                  cegar1 prog' ces' orig_fun_list



let cegar prog orig_fun_list = cegar1 prog [] orig_fun_list
