
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



let rec cegar1 prog preds ces =
  pre ();
  let pr =
    if !Flag.expand_nonrec
    then CEGAR_util.print_prog_typ'
    else CEGAR_print.prog_typ
  in
  let () = Format.printf "Program with abstraction types (CEGAR-cycle %d)::@.%a@." !Flag.cegar_loop pr prog in
  let prog = if !Flag.refine = Flag.RefineSizedType then LazyInterface.instantiate_param prog else prog in
  let labeled,abst = CEGAR_abst.abstract None prog in
  let result = ModelCheck.check None abst prog in
  let result' = apply_opt (fun ce -> CEGAR_trans.trans_ce ce labeled prog) result in
    match result',ces with
        None,_ -> prog, None
      | Some ce, ce'::_ when ce = ce' && not !Flag.use_filter ->
          Format.printf "Filter option enabled.@.";
          Format.printf "Restart CEGAR-loop.@.";
          Flag.use_filter := true;
          cegar1 prog preds ces
      | Some ce, ce'::_ when ce = ce' && not !Flag.use_neg_pred ->
          Format.printf "Negative-predicate option enabled.@.";
          Format.printf "Restart CEGAR-loop.@.";
          Flag.use_neg_pred := true;
          cegar1 prog preds ces
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
                let _,prog' = Refine.refine preds prefix ces' prog in
                  post ();
                  cegar1 prog' preds ces'



type result = Success of prog | Fail of ce

let rec cegar2 prog preds ce_map =
  pre ();
  Flag.use_filter := false;
  Flag.use_neg_pred := false;
  let rec aux c ce_map ces prog =
    let labeled,abst = CEGAR_abst.abstract (Some c) prog in
    let result = ModelCheck.check (Some c) abst prog in
      match result with
          None ->
            Format.printf "Program with abstraction types (CEGAR-cycle %d)::@.%a@."
              !Flag.cegar_loop CEGAR_print.prog_typ prog;
            Success prog
        | Some ce when List.mem ce ces && not !Flag.use_filter ->
            Format.printf "Filter option enabled.@.";
            Flag.use_filter := true;
            aux c ce_map ces prog
        | Some ce when List.mem ce ces && not !Flag.use_neg_pred ->
            Format.printf "Negative-predicate option enabled.@.";
            Flag.use_neg_pred := true;
            aux c ce_map ces prog
        | Some ce when List.mem ce ces ->
            let ce_labeled = get_opt_val result in
            if !Flag.print_eval_abst then CEGAR_trans.eval_abst_cbn prog labeled abst ce_labeled;
            raise NoProgress
        | Some ce when List.mem_assoc ce ce_map ->
            let map = List.assoc ce ce_map in
            let prog' = Refine.add_preds map prog in
              aux (c+1) ce_map (ce::ces) prog'
        | Some ce ->
            Format.printf "Program with abstraction types (CEGAR-cycle %d)::@.%a@."
              !Flag.cegar_loop CEGAR_print.prog_typ prog;
            Fail ce
  in
  let ces,prog' =
    match ce_map with
        [] -> [], prog
      | (ce,map)::_ -> [ce], Refine.add_preds map prog
  in
    match aux 1 ce_map ces prog' with
        Success prog -> prog, None
      | Fail ce ->
          Feasibility.print_ce_reduction ce prog;
          match Feasibility.check ce prog with
              Feasibility.Feasible(_, sol) -> prog, Some (make_ce_printer ce prog sol)
            | Feasibility.Infeasible prefix ->
                let () =
                  if true then
                    Format.printf "Prefix of spurious counter-example::@.%a@.@."
                      CEGAR_print.ce prefix
                in
                let map,_ = Refine.refine preds prefix [ce] prog in
                let ce_map' = (ce,map)::ce_map in
                  post ();
                  cegar2 prog preds ce_map'



let cegar prog preds =
  if !Flag.new_cegar
  then cegar2 prog preds []
  else cegar1 prog preds []








