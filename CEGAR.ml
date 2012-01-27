
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

let pre prog =
  Id.save_counter ();
  Format.printf "Program with abstraction types (CEGAR-cycle %d)::@.%a@." !Flag.cegar_loop CEGAR_print.print_prog_typ prog

let post () =
  incr Flag.cegar_loop;
  (**)
  Wrapper.reopen_cvc3 ();
  (**)
  Id.reset_counter ()

let rec cegar1 prog preds ces =
  let () = pre prog in
  let abst = CEGAR_abst.abstract prog in
  let result = ModelCheck.check abst prog in
    match result,ces with
        None,_ -> prog, None
      | Some ce, ce'::_ when ce = ce' ->
          if !Flag.print_eval_abst then CEGAR_abst.eval_abst_cbn prog abst ce;
          raise NoProgress
      | Some ce, _ ->
          if !Flag.print_eval_abst then CEGAR_abst.eval_abst_cbn prog abst ce;
          Feasibility.print_ce_reduction ce prog;
          match Feasibility.check ce prog with
              Feasibility.Feasible (env, sol) -> prog, Some (make_ce_printer ce prog sol)
            | Feasibility.Infeasible prefix ->
                let ces' = ce::ces in
                let _,prog' = Refine.refine preds prefix ce prog in
                  post ();
                  cegar1 prog' preds ces'


let rec cegar2 prog preds map = assert false
  (*
  let () = pre prog in
  let rec aux ces' prog =
    let abst = CEGAR_abst.abstract prog in
    let result = ModelCheck.check abst prog in
      match result with
          None -> prog
        | Some _ -> assert false
  in
    match result,map with
        None,_ -> prog, None
      | Some ce, (ce',_)::_ when ce = ce' ->
          if !Flag.print_eval_abst then CEGAR_abst.eval_abst_cbn prog abst ce;
          raise NoProgress
      | Some ce, _ ->
          if !Flag.print_eval_abst then CEGAR_abst.eval_abst_cbn prog abst ce;
          Feasibility.print_ce_reduction ce prog;
          match Feasibility.check ce prog with
              Feasibility.Feasible(_, sol) -> prog, Some (make_ce_printer ce prog sol)
            | Feasibility.Infeasible prefix ->
                let map,prog' = Refine.refine preds prefix ce prog in
                let ces' = (ce,map)::ces in
                  post ();
                  cegar2 prog' preds ces'
  *)


let cegar prog preds =
  if true
  then cegar1 prog preds []
  else cegar2 prog preds []
