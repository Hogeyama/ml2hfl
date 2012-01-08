
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
  Wrapper2.reopen_cvc3 ();
  (**)
  Id.reset_counter ()


(*
let test_prog =
  ([],
   ["f",["x";"y"],Const True,[],
    make_if
      (make_eq_int (Var"x") (make_int 0))
      (Const Unit)
      (make_app (Var "f") [make_sub (Var "x") (make_int 1); Var "y"]);
    "main",[],Const True,[],make_app (Var "f") [make_int 1; make_int 1]],
   "main")

let test_prog =
  ([],
   ["f",["x"],Const True,[],
      (make_app (Var "f") [make_int 0]);
    "main",[],Const True,[],make_app (Var "f") [make_int 1]],
   "main")
let test_prog =
  ([],
   ["f",["g"],Const True,[],App(Var "g", make_int 1);
    "g1",["x"],Const True,[],make_int 0;
    "g2",["x"],Const True,[],Var "x";
    "main",[],Const True,[], make_if (make_eq_int (App(Var "f", Var "g1")) (App(Var "f", Var "g2"))) (Const Unit) (Const Unit)],
   "main")

let test_prog =
  ([],
   ["f",["x"],Const True,[Branch 0],Var "x";
    "bot",[],Const True,[],Const Unit;
    "main",[],Const True,[], App(App(App (Const If, Const True), Var "bot"), (App(Var "f", Const Unit)))],
   "main")

let test_prog = Typing.infer test_prog
let test_prog' = Useless_elim.elim test_prog
let () = Format.printf "TEST:@.%a@.@." CEGAR_print.prog test_prog
let () = Format.printf "TEST_ELIMED:@.%a@.@." CEGAR_print.prog test_prog'
let () = assert false
*)

let rec cegar prog preds ces =
  let () = pre prog in
  let abst = CEGAR_abst.abstract prog in
  let result = ModelCheck.check abst prog in
    match result,ces with
        None,_ -> prog, None
      | Some ce, ce'::_ when ce = ce' ->
          if !Flag.print_eval_abst then CEGAR_abst.eval_abst_cbn prog abst ce;
          raise NoProgress
      | Some ce, _ ->
          Feasibility.print_ce_reduction ce prog;
          match Feasibility.check ce prog with
              Feasibility.Feasible (env, sol) -> prog, Some (make_ce_printer ce prog sol)
            | Feasibility.Infeasible prefix ->
                let ces' = ce::ces in
                let prog' = Refine.refine preds prefix ces' prog in
                  post ();
                  cegar prog' preds ces'

