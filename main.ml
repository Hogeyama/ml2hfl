
open Util

exception TimeOut
exception LongInput
exception IllegalInput
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


let rec cegar t1 t2 ce_prev =
  let () = if Flag.print_type then Format.printf "Program with abstraction types (CEGAR-cycle %d):@.%a\n" !Flag.cegar_loop (Syntax.print_term_fm_break Syntax.ML true) t1 in
  let n = Syntax.get_counter () in

  let () = if Flag.print_progress then print_msg "\n(1) Abstracting ... " in
  let tmp = get_time() in
  let t_abst = (*Syntax.part_eval*) (*Syntax.eval*) (Abstract.abstract t1) in
  let () = add_time tmp Flag.time_abstraction in
  let () = if Flag.print_progress then print_msg "DONE!\n" in

  let () = if Flag.print_abst_eager then Format.printf "Abstracted Program:@.%a" (Syntax.print_term_fm Syntax.ML false) t_abst in
  let () = Format.pp_print_flush Format.std_formatter () in

  let () = if Flag.print_progress then print_msg  "\n(2) Checking HORS ... " in
  let tmp = get_time() in
  let result = Check.model_check t_abst in
  let () = add_time tmp Flag.time_mc in
  let () = if Flag.print_progress then print_msg "DONE!\n" in

    match result with
        None -> t1, None
      | Some ce ->
          let () = print_msg "Spurious counter-example:\n" in
          let () = List.iter (fun b -> if b then print_msg "then --> " else print_msg "else --> ") ce in
          let () = print_msg "fail\n\n" in
          try
            let () = if Flag.print_progress then print_msg "\n(3) Checking CE and Discovering predicates ... " in
            let tmp = get_time () in
            let t'',t1',t2' =
              if Some ce = ce_prev
              then
                match t2 with
                    None -> raise CannotDiscoverPredicate
                  | Some t2 ->
                      let t1' = Syntax.copy_pred t1 t2 in
                      let defs,t = Syntax.lift t1' in
                      let pred,ce' = Feasibility.check ce defs t in
                      let t1'' = Refine.refine ce' defs t t1' pred in
                        t1'', t2, None
              else
                let defs,t' = Syntax.lift t1 in
                let pred,ce' = Feasibility.check ce defs t' in
                let () = if Flag.debug then Format.printf "ce:%d, ce':%d" (List.length ce) (List.length ce') in
                let t'' = Refine.refine ce' defs t' t1 pred in
                  t'', t1, t2
            in
            let t1'',t2'' =
              if t'' = t1'
              then
                match t2' with
                    None -> raise CannotDiscoverPredicate
                  | Some t2 ->
                      let t1' = Syntax.copy_pred t1' t2 in
                      let defs,t = Syntax.lift t1' in
                      let pred,ce' = Feasibility.check ce defs t in
                      let t1'' = Refine.refine ce' defs t t1' pred in
                        t1'', None
              else t'', t2'
            in
              add_time tmp Flag.time_cegar;
              if Flag.print_progress then print_msg "DONE!\n";
              Syntax.set_counter n;
              incr Flag.cegar_loop;
              cegar t1'' t2'' (Some ce)
          with
              Syntax.Feasible p -> t1, Some (ce,p)
            | Syntax.Infeasible -> t1, None


let print_ce ce t =
  let str = Flag.string_for_result in
  let t' = Syntax.add_string str t in
  let t'' = CPS.trans t' in
  let t''' = Typing.typing t'' in
  let trace = Syntax.get_trace ce t''' in
  let print_var_bool = function
      Syntax.True -> print_msg "-then->\n"
    | Syntax.False -> print_msg "-else->\n"
    | Syntax.Var x ->
        begin
          try
            if String.sub x.Syntax.origin (String.length x.Syntax.origin - String.length str) (String.length str) = str
            then Format.printf "%s -->\n" (String.sub x.Syntax.origin 0 (String.length x.Syntax.origin - String.length str))
          with
              Invalid_argument "String.sub" -> ()
        end
    | _ -> assert false
  in
    List.iter print_var_bool trace;
    Format.printf "error";
    Format.printf "\n\n"


let main in_channel =
  let input_string = String.create Flag.max_input_size in
  let n = input in_channel input_string 0 Flag.max_input_size in
  let () = if n = Flag.max_input_size then raise LongInput in
  let input_string = String.sub input_string 0 n in
  let () = if Flag.web then write_log_string input_string in
  let parsed =
    try
      Parser.file Lexer.token (Lexing.from_string input_string)
    with _ -> raise IllegalInput
  in
  let free_variables,alpha = Alpha.alpha parsed in
  let () = if Flag.web then write_log_term alpha in
  let () = if Flag.print_source then Format.printf "Source Program:@.%a\n" (Syntax.print_term_fm_break Syntax.ML false) alpha else () in
  let cps = (*Syntax.eval*) (CPS.trans alpha) in
  let () = if Flag.print_cps then Format.printf "CPS-converted Program:@.%a\n" (Syntax.print_term_fm_break Syntax.ML false) cps in

  let cps' = Syntax.trans cps in

  let cps' = Typing.typing cps' in
(*
  let cps' = Syntax.eta_expand cps' in
*)

  let cps1 =
    let defs, t = Syntax.lift cps' in
(*
      Typing.typing_defs defs t;
*)
      List.fold_right
        (fun (f, (xs, t')) t ->
           if List.exists (fun id -> List.mem_assoc id defs) (Syntax.get_fv t') then
             Syntax.Letrec(f,xs,t',t)
           else
             Syntax.Let(f,xs,t',t))
        defs t
  in
  let cps2 =
    let defs, t = Syntax.lift2 cps' in
      List.fold_right
        (fun (f, (xs, t')) t ->
           if List.exists (fun id -> List.mem_assoc id defs) (Syntax.get_fv t') then
             Syntax.Letrec(f,xs,t',t)
           else
             Syntax.Let(f,xs,t',t))
        defs t
  in

  let t_result, result = cegar cps1 (Some cps2) None in
    match result with
        None -> print_msg "\nSafe!\n\n"
      | Some (ce,p) ->
          let is_free t =
            let fv = Syntax.get_fv t in
            let ids = List.map (fun x -> x.Syntax.id) free_variables in
              List.for_all (fun x -> List.mem x.Syntax.id ids) fv
          in
          let ts = Wrapper.get_solution p in
          let ts' = List.filter is_free ts in
          let aux t =
            Syntax.print_term Syntax.ML false t;
            print_msg "; "
          in
            print_msg "Unsafe!\n\n";
            print_msg "Error trace:\n";
            List.iter aux ts';
            if List.length ts' <> 0 then print_msg "\n";
            print_ce ce alpha



let () = 
  if !Sys.interactive
  then ()
  else
    try
      let cin = try open_in Sys.argv.(1) with Invalid_argument "index out of bounds" -> stdin in
        if Flag.web then open_log ();
        Wrapper.open_cvc3 ();
        Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise TimeOut));
        ignore (Unix.alarm Flag.time_limit);
        main cin;
        print_info ();
        Wrapper.close_cvc3 ();
        if Flag.web then close_log ()
    with
        Typing.CannotUnify -> Format.printf "Cannot unify.@."; exit 1
      | IllegalInput -> Format.printf "Illegal input.@."; exit 1
      | LongInput -> Format.printf "Input is too long.@."; exit 1
      | TimeOut -> Format.printf "Verification failed (time out).@."; exit 1
      | CannotDiscoverPredicate -> Format.printf "Verification failed (cannot discover necessary predicates).@."; exit 1
