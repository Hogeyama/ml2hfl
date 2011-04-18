
open Util

exception TimeOut
exception LongInput
exception NoProgress
exception CannotDiscoverPredicate

let log_filename = ref ""
let log_cout = ref stdout
let log_fm = ref Format.std_formatter

let open_log () =
  log_filename := Filename.basename (Filename.temp_file "log" ".ml");
  log_cout := open_out (Flag.log_dir ^ !log_filename);
  log_fm := Format.formatter_of_out_channel !log_cout
let close_log () =
  close_out !log_cout

let write_log_string s =
  Format.fprintf !log_fm "%s\n@." s
let write_log_term t =
  Syntax.print_term_fm_break Syntax.ML false !log_fm t;
  flush !log_cout


let print_info () =
  Format.printf "cycle: %d\n" !Flag.cegar_loop;
  Format.printf "abst: %fsec\n" !Flag.time_abstraction;
  Format.printf "mc: %fsec\n" !Flag.time_mc;
  Format.printf "cegar: %fsec\n" !Flag.time_cegar;
  if Flag.debug then Format.printf "IP: %fsec\n" !Flag.time_interpolant;
  Format.printf "total: %fsec\n" (get_time());
  Format.pp_print_flush Format.std_formatter ()


let rec cegar tdefs t1 ce_prev =
  let () = if Flag.print_type then Format.printf "Program with abstraction types (CEGAR-cycle %d):@.%a\n" !Flag.cegar_loop (Syntax.print_term_fm_break Syntax.ML true) t1 in
  let n = Syntax.get_counter () in
  let () = if Flag.print_progress then print_msg "\n(1) Abstracting ... " in
  let tmp = get_time() in
  let t_abst = Abstract.abstract t1 in
  let () = add_time tmp Flag.time_abstraction in
  let () = if Flag.print_progress then print_msg "DONE!\n" in

  let () = if Flag.print_abst_eager then Format.printf "Abstracted Program:@.%a@.@." (Syntax.print_term_fm Syntax.ML false) t_abst in
  let () = Format.pp_print_flush Format.std_formatter () in

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
                (*
                  let () = Format.printf "ce:%d ce':%d@." (List.length ce) (List.length ce') in
                *)
              let ce = ce' in
              let () = if Flag.print_progress then print_msg "\n(3) Checking CE and Discovering predicates ... " in
              let tmp = get_time () in
              let t'' =
                if ce_prev <> [] && ce = List.hd ce_prev
                then
                  raise NoProgress
                else
                  let () = if false && Flag.debug then Format.printf "The length of counterexample: %d@.@." (List.length ce) in
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
                Syntax.set_counter n;
                cegar tdefs t'' (ce::ce_prev)
            with
                Syntax.Feasible p -> t1, Some (ce,p)
              | Syntax.Infeasible -> raise CannotDiscoverPredicate(*t1, None*)

(*
let print_ce ce t =
  let t' = CPS.trans t in
  let trace = Syntax.get_trace ce t' in
  let print_var_bool = function
      Syntax.True -> print_msg "-then->"
    | Syntax.False -> print_msg "-else->"
    | Syntax.Var x ->
        begin
          try
            if String.sub x.Syntax.name (String.length x.Syntax.name - String.length str) (String.length str) = str
            then Format.printf "%s -->@." (String.sub x.Syntax.name 0 (String.length x.Syntax.name - String.length str))
          with
              Invalid_argument "String.sub" -> ()
        end
    | Syntax.Event(s) ->
        print_msg ("-" ^ s ^ "->")
    | _ -> assert false
  in
    List.iter print_var_bool trace;
    print_msg "error";
    print_msg ""
*)

let main filename in_channel =
  let input_string = String.create Flag.max_input_size in
  let n = input in_channel input_string 0 Flag.max_input_size in
  let () = if n = Flag.max_input_size then raise LongInput in
  let input_string = String.sub input_string 0 n in

  let () = if Flag.web then write_log_string input_string in
  let tdefs, parsed =
    let lb = Lexing.from_string input_string in
    let _ = lb.Lexing.lex_curr_p <-
      {Lexing.pos_fname = Filename.basename filename;
       Lexing.pos_lnum = 1;
       Lexing.pos_cnum = 0;
       Lexing.pos_bol = 0}
    in
      [], Parser_wrapper.from_use_file (Parser.use_file Lexer.token lb)
  in
  let alpha = Alpha.alpha parsed in
  let typed = Typing.typing alpha in
  let typed = Syntax.set_target typed in
  let () = if Flag.web then write_log_term typed in
  let () = if Flag.print_source then Format.printf "Source Program:@.%a\n@." Syntax.pp_print_term typed in
  let cps = CPS.trans typed in
  let () = if Flag.print_cps then Format.printf "CPS-converted Program:@.%a\n@." Syntax.pp_print_term cps in
  let cps =
    let defs, t = Syntax.lift2 cps in
      (* Typing.typing_defs defs t; *)
      List.fold_right
        (fun (f, (xs, t')) t ->
           let flag =
             if List.exists (fun id -> List.mem_assoc id defs) (Syntax.get_fv t')
             then Syntax.Recursive
             else Syntax.Nonrecursive
           in
             Syntax.Let(flag, f,xs,t', t))
        defs t
  in

  let tdefs = List.map (fun (x, t) -> x, Syntax.fff t) tdefs in
  let cps = Refine.add_preds_ tdefs cps in
  let () = Wrapper.set_datatype_cvc3 cps in
  let t_result, result = cegar tdefs cps [] in
    match result with
        None -> print_msg "\nSafe!\n\n"
      | Some (ce,p) ->
          let sol = Wrapper.get_solution p t_result in
            print_msg "Unsafe!\n";
            print_msg "Error trace:";
            List.iter (fun t -> Format.printf "%s; " t) sol;
            if List.length sol <> 0 then Format.printf "@.";
            Syntax.print_ce ce typed



let () = 
  if !Sys.interactive
  then ()
  else
    let filename = if Array.length Sys.argv >= 2 then Sys.argv.(1) else "-" in
    let cin = if filename = "-" then stdin else open_in filename in
      try
        if Flag.web then open_log ();
        Wrapper.open_cvc3 ();
        Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise TimeOut));
        ignore (Unix.alarm Flag.time_limit);
        main Sys.argv.(1) cin;
        print_info ();
        Wrapper.close_cvc3 ();
        if Flag.web then close_log ()
      with
          Typing.CannotUnify -> Format.printf "Cannot unify.@."; exit 1
        | Parsing.Parse_error _ -> Format.printf "Parse error.@."; exit 1
        | Typecore.Error (_,e) -> Format.printf "%a@." Typecore.report_error e; exit 1
        | LongInput -> Format.printf "Input is too long.@."; exit 1
        | TimeOut -> Format.printf "Verification failed (time out).@."; exit 1
        | NoProgress -> Format.printf "Verification failed (new error path not found).@."; exit 1
        | CannotDiscoverPredicate -> Format.printf "Verification failed (new predicate not found).@."; exit 1
