
open Util
open CEGAR_const
open CEGAR_syntax

exception NoProgress
exception CannotDiscoverPredicate

(*
val refine : prog -> int list list -> (var * unit Type.t) list -> (var * t Type.t) list
*)


































let rec cegar tdefs t0 ce_prev =
  let t = trans_prog t0 in
  let () = Format.printf "Program with abstraction types (CEGAR-cycle %d):@.%a\n"
    !Flag.cegar_loop CEGAR_print.print_prog t
  in
  let n = Id.get_counter () in
  let () = if Flag.print_progress then print_msg "\n(1) Abstracting ... " in
  let tmp = get_time() in
  let t_abst = Abstract.abstract t in
  let () = add_time tmp Flag.time_abstraction in
  let () = if Flag.print_progress then print_msg "DONE!\n" in


  let () = if Flag.print_progress then print_msg  "\n(2) Checking HORS ... " in
  let tmp = get_time() in
  let result = Check.model_check t_abst in
  let () = add_time tmp Flag.time_mc in
  let () = if Flag.print_progress then print_msg "DONE!\n" in

    match result with
        None -> t1, None
      | Some ce ->
          let () = print_msg "Spurious counter-example:" in
          let () = List.iter (fun node -> print_msg (Syntax.string_of_node node ^ " --> ")) ce in
          let () = print_msg ".\n" in
          let ce =
            let rec aux a = function
                [] -> assert false
              | [t] -> a @ [Syntax.FailNode]
              | t::ce -> aux (a @ [t]) ce
            in
              aux [] ce
          in
            try
              let ce' =
                if Flag.use_prefix_trace then
                  let defs,t = Syntax.lift t1 in
                    Feasibility.get_prefix ce defs t
                else
                  ce
              in
              let ce' =
                let rec aux a = function
                    [] -> assert false
                  | [Syntax.LabNode true] -> a @ [Syntax.EventNode "then_fail"]
                  | [Syntax.LabNode false] -> a @ [Syntax.EventNode "else_fail"]
                  | [Syntax.PatNode i] -> a @ [Syntax.EventNode ("br" ^ string_of_int i ^ "_fail")]
                  | [t] -> a @ [t]
                  | t::ce -> aux (a @ [t]) ce
                in
                  aux [] ce'
	          in
              let ce = ce' in
              let () = if Flag.print_progress then print_msg "\n(3) Checking CE and Discovering predicates ... " in
              let tmp = get_time () in
              let t'' =
                if ce_prev <> [] && ce = List.hd ce_prev
                then
                  raise NoProgress
                else
                  let () =
                    if false && Flag.debug
                    then Format.printf "The length of counterexample: %d@.@." (List.length ce)
                  in
                  let t1 =
                    if !Flag.merge_counterexample then
                      Refine.add_preds_ tdefs (Refine.remove_preds t1)
                    else
                      t1
                  in
                    try
                      Refine.refine tdefs (if !Flag.merge_counterexample then ce::ce_prev else [ce]) t1
                    with Infer.Untypable ->
                      let defs,t' = Syntax.lift t1 in
                        Feasibility.check ce defs t'; raise Syntax.Infeasible
              in
                add_time tmp Flag.time_cegar;
                if Flag.print_progress then print_msg "DONE!\n";
                incr Flag.cegar_loop;
                (**)
                Wrapper.close_cvc3 ();
                Wrapper.open_cvc3 ();
                Id.set_counter n;
                cegar tdefs t'' (ce::ce_prev)
            with
                Syntax.Feasible p -> t1, Some (ce,p)
              | Syntax.Infeasible -> raise CannotDiscoverPredicate(*t1, None*)




