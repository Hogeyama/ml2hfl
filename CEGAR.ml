
open Utilities
open CEGAR_syntax
open CEGAR_util

exception NoProgress
exception CannotDiscoverPredicate


let rec cegar prog ces =
  let n = Id.get_counter () in
  let () = Format.printf "Program with abstraction types (CEGAR-cycle %d)::@.%a\n"
    !Flag.cegar_loop CEGAR_print.print_prog_typ prog
  in
  let () = if Flag.print_progress then print_msg "\n(1) Abstracting ... " in
  let tmp = get_time() in
  let abst =
    match !Flag.refine with
        Flag.RefineDependentType -> CEGAR_abst_CPS.abstract prog
      | Flag.RefineSizedType -> CEGAR_abst.abstract prog
  in
  let () = if false then Format.printf "Abstracted program::\n%a@." CEGAR_print.print_prog abst in
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
  let () = if Flag.print_progress then print_msg "DONE!\n\n" in

    match result,ces with
        None,_ -> prog, None
      | Some ce, ce'::ces when ce = ce' -> raise NoProgress;
      | Some ce, _ ->
          Format.printf "Spurious counter-example::\n%a\n@." CEGAR_print.print_ce ce;
          let () = if Flag.print_progress then print_msg "\n(3) Checking CE and Discovering predicates ... " in
          let tmp = get_time () in
            match Feasibility.check ce prog with
                Feasibility.Feasible (env, sol) ->
                  let print () =
                    Format.printf "Inputs:@.";
                    List.iter (fun t -> Format.printf "  %s;@." t) sol;
                    Feasibility.print_ce_reduction ce prog
                  in
                    prog, Some print
              | Feasibility.Infeasible prefix ->
                  let ces' =
                    if Flag.use_prefix_trace
                    then
                      let prefix' =
                        match !Flag.refine with
                            Flag.RefineDependentType ->
                              let rec aux = function
                                  [] -> []
                                | [BrNode true; LineNode _] -> [EventNode "then_fail"]
                                | [BrNode false; LineNode _] -> [EventNode "else_fail"]
                                | n::ce -> n :: aux ce
                              in
                                aux prefix
                          | _ -> prefix
                      in
                        Format.printf "\nPrefix of spurious counter-example::\n%a\n@." CEGAR_print.print_ce prefix';
                        prefix'::ces
                    else ce::ces
                  in
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




