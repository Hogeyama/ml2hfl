
open Utilities

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
  Syntax.print_term false !log_fm t;
  flush !log_cout


let print_info () =
  Format.printf "cycle: %d\n" !Flag.cegar_loop;
  Format.printf "abst: %fsec\n" !Flag.time_abstraction;
  Format.printf "mc: %fsec\n" !Flag.time_mc;
  Format.printf "cegar: %fsec\n" !Flag.time_cegar;
  if Flag.debug then Format.printf "IP: %fsec\n" !Flag.time_interpolant;
  Format.printf "total: %fsec\n" (get_time());
  Format.pp_print_flush Format.std_formatter ()



let spec_file = ref ""

let main filename in_channel =
  let input_string =
    let s = String.create Flag.max_input_size in
    let n = my_input in_channel s 0 Flag.max_input_size in
      if n = Flag.max_input_size then raise LongInput;
      String.sub s 0 n
  in

  let () = if !Flag.web then write_log_string input_string in
  let t =
    let lb = Lexing.from_string input_string in
    let _ = lb.Lexing.lex_curr_p <-
      {Lexing.pos_fname = Filename.basename filename;
       Lexing.pos_lnum = 1;
       Lexing.pos_cnum = 0;
       Lexing.pos_bol = 0}
    in
      Parser_wrapper.from_use_file (Parser.use_file Lexer.token lb)
  in

  let () = if true then Format.printf "parsed::@.%a@.@.@." Syntax.pp_print_term t in

  let spec =
    if !spec_file = ""
    then []
    else
      Spec_parser.typedefs Spec_lexer.token (Lexing.from_channel (open_in !spec_file))
  in

  let () =
    if spec <> []
    then
      begin
        Format.printf "spec::@.";
        List.iter (fun (x,typ) -> Format.printf "%a: %a@." Syntax.print_id x Syntax.print_typ typ) spec;
        Format.printf "@."
      end
  in

  let t = if !Flag.cegar = Flag.CEGAR_DependentType then Trans.set_target t else t in
  let () = if true then Format.printf "set_target::@.%a@.@.@." Syntax.pp_print_term t in
  let t =
    if !Flag.init_trans
    then
      let t = Trans.copy_poly_funs t in
      let () = if true then Format.printf "copy_poly::@.%a@.@." Syntax.pp_print_term' t in
      let spec' = Trans.rename_spec spec t in
        Format.printf "spec'::@.";
        List.iter (fun (x,typ) -> Format.printf "%a: %a@." Syntax.print_id x Syntax.print_typ typ) spec';
        Format.printf "@.";
      let t = Trans.replace_typ spec' t in
      let () = if true then Format.printf "add_preds::@.%a@.@.@." Syntax.pp_print_term' t in
      let t = Abstract.abstract_recdata t in
      let () = if false then Format.printf "abst_recdata::@.%a@.@." Syntax.pp_print_term t in
      let t = Abstract.abstract_list t in
      let () = if true then Format.printf "abst_list::@.%a@.@." Syntax.pp_print_term t in
      let t = CPS.trans t in
      let () = if true then Format.printf "CPS::@.%a@.@.@." Syntax.pp_print_term t in
      let t = CPS.remove_pair t in
      let () = if true then Format.printf "remove_pair::@.%a@.@.@." Syntax.pp_print_term t in
        t
    else t
  in

  let () = Type_check.check t Type.TUnit in
  let prog,preds = CEGAR_util.trans_prog t in
    match !Flag.cegar with
        Flag.CEGAR_SizedType -> LazyInterface.verify prog
      | Flag.CEGAR_DependentType ->
          let t_result, result = CEGAR.cegar prog preds [] in
	    match result with
	        None -> Format.printf "@.Safe!@.@."
	      | Some print ->
                  Format.printf "@.@.Unsafe!@.@.";
                  print ()


let usage =  "Usage: " ^ Sys.executable_name ^ " [options] file\noptions are:"
let arg_spec =
  ["-web", Arg.Set Flag.web, " web mode";
   "-I", Arg.String (fun dir -> Config.load_path := dir::!Config.load_path),
         "<dir>  add <dir> to the list of include directories";
   "-st", Arg.Unit (fun _ -> Flag.cegar := Flag.CEGAR_SizedType), " use sized type system for CEGAR";
   "-c", Arg.Unit (fun _ -> Flag.cegar := Flag.CEGAR_SizedType), " same as -st";
   "-na", Arg.Clear Flag.init_trans, " do not abstract data";
(*
   "-exn", Arg.Set Flag.cps_excep, " CPS transformation for exception";
*)
   "-rs", Arg.Unit (fun _ -> Flag.refine := Flag.RefineSizedType), " use sized type system for predicate discovery";
   "-rd", Arg.Unit (fun _ -> Flag.refine := Flag.RefineDependentType), " use dependent type system for predicate discovery";
   "-spec", Arg.String (fun file -> spec_file := file),
         "<filename>  use <filename> as specification";
   "-ea", Arg.Unit (fun _ -> Flag.print_eval_abst := true), " print evaluation of abstacted program";
   "-lift-fv", Arg.Unit (fun _ -> Flag.lift_fv_only := true), " lift variables which occur in a body";
  ]


let () =
  if !Sys.interactive
  then ()
  else
    try
      let filename = ref "" in
      let set_file name =
        if !filename <> "" then (Arg.usage arg_spec usage; exit 1);
        filename := name
      in
      let () = Arg.parse arg_spec set_file usage in
      let cin = match !filename with ""|"-" -> stdin | _ -> open_in !filename in
        if !Flag.web then open_log ();
        Wrapper.open_cvc3 ();
        Wrapper2.open_cvc3 ();
        Cvc3Interface.open_cvc3 ();
        Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise TimeOut));
        ignore (Unix.alarm Flag.time_limit);
        main !filename cin;
        print_info ();
        Cvc3Interface.close_cvc3 ();
        Wrapper2.close_cvc3 ();
        Wrapper.close_cvc3 ();
        if !Flag.web then close_log ()
    with
        Parsing.Parse_error -> Format.printf "Parse error.@."; exit 1
      | LongInput -> Format.printf "Input is too long.@."; exit 1
      | TimeOut -> Format.printf "Verification failed (time out).@."; exit 1
      | CEGAR.NoProgress -> Format.printf "Verification failed (new error path not found).@."; exit 1
      | Refine.CannotRefute -> Format.printf "Verification failed (new predicates not found).@."; exit 1
      | Typecore.Error (_,e) -> Format.printf "%a@." Typecore.report_error e; exit 1
      | Typemod.Error(_,e) -> Format.printf "%a@." Typemod.report_error e; exit 1
      | Env.Error e -> Format.printf "%a@." Env.report_error e; exit 1
      | Typetexp.Error(_,e) -> Format.printf "%a@." Typetexp.report_error e; exit 1
