
open Utilities
open CEGAR_syntax

exception NoProgress
exception CannotDiscoverPredicate


let rec cegar prog ces =
  let _ = Typing.infer prog in
  let n = Id.get_counter () in
  let () = Format.printf "Program with abstraction types (CEGAR-cycle %d):@.%a\n"
    !Flag.cegar_loop CEGAR_print.print_prog_typ prog
  in
  let () = if Flag.print_progress then print_msg "\n(1) Abstracting ... " in
  let tmp = get_time() in
  let abst =
    match !Flag.refine with
        Flag.RefineDependentType -> CEGAR_abst_CPS.abstract prog
      | Flag.RefineSizedType -> CEGAR_abst.abstract prog
  in
  let () = Format.printf "Abstracted program:\n%a@." CEGAR_print.print_prog abst in
  let () = add_time tmp Flag.time_abstraction in
  let () = if Flag.print_progress then print_msg "DONE!\n" in
  let () = if Flag.print_progress then print_msg  "\n(2) Checking HORS ... " in
  let tmp = get_time() in
  let def_num = (fun (_,defs,_) -> List.length defs) prog in
  let result =
    match !Flag.refine with
        Flag.RefineDependentType -> ModelCheck_CPS.check abst def_num
      | Flag.RefineSizedType -> ModelCheck.check abst def_num
  in
  let () = add_time tmp Flag.time_mc in
  let () = if Flag.print_progress then print_msg "DONE!\n" in

    match result,ces with
        None,_ -> prog, None
      | Some ce, ce'::ces when ce = ce' -> raise NoProgress;
      | Some ce, _ ->
          Format.printf "Spurious counter-example:\n%a\n@." (print_list Format.pp_print_int "; " false) ce;
          try
            let ces' = ce::ces in
            let () = if Flag.print_progress then print_msg "\n(3) Checking CE and Discovering predicates ... " in
            let tmp = get_time () in
            let prog' = Refine.refine ces' prog in
              add_time tmp Flag.time_cegar;
              if Flag.print_progress then print_msg "DONE!\n";
              incr Flag.cegar_loop;
              (**)
              Wrapper2.close_cvc3 ();
              Wrapper.close_cvc3 ();
              Wrapper.open_cvc3 ();
              Wrapper2.open_cvc3 ();
              (**)
              Id.set_counter n;
              cegar prog' ces'
          with Refine.CannotRefute ->
            let b,constr = Feasibility.check ce prog in
              if b
              then prog, Some (ce,constr)
              else raise CannotDiscoverPredicate(*t1, None*)




