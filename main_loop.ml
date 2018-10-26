open Util
open Mochi_util

type result = Safe of (Syntax.id * Ref_type.t) list | Unsafe of int list

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)



let cegar_of_preprocessed ?fun_list spec results =
  let set_main = Option.map fst @@ List.assoc_option Preprocess.Set_main results in
  let main = Option.(set_main >>= return-|Problem.term >>= Trans.get_set_main)  in
  let problem = Preprocess.last_problem results in
  let fun_list' =
    match fun_list with
    | None -> Term_util.get_top_funs @@ Problem.term Preprocess.(take_result Decomp_pair_eq results)
    | Some fun_list' -> fun_list'
  in

  let prog,map,_,make_get_rtyp_trans = CEGAR_trans.trans_prog problem in
  let abst_cegar_env =
    Spec.get_abst_cegar_env spec prog
    |@> Verbose.printf "%a@." Spec.print_abst_cegar_env
  in
  let prog = CEGAR_trans.add_env abst_cegar_env prog in
  let make_get_rtyp =
    if !!Debug.check then
      let aux f (label,(_,g)) map x =
        Format.printf "BEGIN[%s]@." @@ Preprocess.string_of_label label;
        let r =
          try
            g (f map) x
          with e ->
            Format.printf "GET_RTYP ERROR[%s]: %s@." (Preprocess.string_of_label label) (Printexc.to_string e);
            assert false
        in
        Format.printf "END %s@." @@ Preprocess.string_of_label label;
        r
      in
      List.fold_left aux make_get_rtyp_trans results
    else
      List.fold_left (fun f (_,(_,g)) -> g -| f) make_get_rtyp_trans results
  in

  let info =
    let orig_fun_list =
      let aux x = List.assoc_option (CEGAR_trans.trans_var x) map in
      List.filter_map aux fun_list'
    in
    let inlined = List.map CEGAR_trans.trans_var spec.Spec.inlined in
    let fairness =
      if Flag.Method.(!mode = FairNonTermination) then
        Some spec.Spec.fairness
      else
        None
    in
    CEGAR_syntax.{prog.info with orig_fun_list; inlined; fairness}
  in
  CEGAR_syntax.{prog with info}, make_get_rtyp, set_main, main


let run_preprocess ?make_pps spec problem =
  let pps' =
    match make_pps with
    | None -> Preprocess.all spec
    | Some make_pps' -> make_pps' spec
  in
  Preprocess.run pps' problem


let write_annot env orig =
  env
  |> List.map (Pair.map_fst Id.name)
  |> WriteAnnot.f !Flag.mainfile orig

let report_safe env orig {Problem.term=t0} =
  if !Flag.PrettyPrinter.write_annot then Option.iter (write_annot env) orig;

  let s =
    match !Flag.Method.mode with
    | Flag.Method.NonTermination -> "Non-terminating!"
    | Flag.Method.FairNonTermination -> "Fair Infinite Execution found!"
    | _ -> "Safe!"
  in
  Color.printf Color.Bright "%s@.@." s;

  if !Flag.Method.relative_complete then
    begin
      let map =
        List.map
          (fun (x, t) ->
           Id.make (-1) (Fpat.Idnt.string_of x) [] Type.Ty.int,
           CEGAR_trans.trans_inv_term @@ FpatInterface.inv_term @@ t)
          !Fpat.RefTypInfer.prev_sol
      in
      let t = Term_util.subst_map map t0 in
      Format.printf "Problem with Quantifiers Added:@.";
      Format.printf "  @[<v>%a@]@.@." Print.term t
    end;

  if env <> [] && Flag.Method.(!mode <> Termination) then
    begin
      Verbose.printf "Refinement Types:@.";
      let env' = List.map (Pair.map_snd Ref_type.simplify) env in
      let pr (f,typ) = Verbose.printf "  %s: %a@." (Id.name f) Ref_type.print typ in
      List.iter pr env';
      Verbose.printf "@.";

      if !Flag.Print.abst_typ then
        begin
          Verbose.printf "Abstraction Types:@.";
          let pr (f,typ) = Verbose.printf "  %s: %a@." (Id.name f) Print.typ @@ Ref_type.to_abst_typ typ in
          List.iter pr env';
          Verbose.printf "@."
        end
    end


let report_unsafe main ce set_main =
  Color.printf Color.Bright "%s@.@." !Flag.Log.result;
  if !Flag.use_abst = [] then
    let pr main_fun =
      let arg_num = Type.arity @@ Id.typ main_fun in
      if arg_num > 0 then
        Format.printf "Input for %a:@.  %a@." Id.print main_fun (print_list Format.pp_print_int "; ") (List.take arg_num ce)
    in
    Option.may pr main;
    match set_main with
    | None -> ()
    | Some set_main -> Format.printf "@[<v 2>Error trace:%a@." Eval.print (ce,set_main)


let rec run_cegar prog =
  try
    match CEGAR.run prog with
    | CEGAR.Safe env ->
        Flag.Log.result := "Safe";
        set_status "Done: Safe";
        Color.printf Color.Bright "Safe!@.@.";
        true
    | CEGAR.Unsafe _ ->
        Flag.Log.result := "Unsafe";
        set_status "Done: Unsafe";
        Color.printf Color.Bright "Unsafe!@.@.";
        false
  with
  | Fpat.RefTypInfer.FailedToRefineTypes when Flag.Method.(not !insert_param_funarg && not !no_exparam) ->
      Flag.Method.insert_param_funarg := true;
      run_cegar prog
  | Fpat.RefTypInfer.FailedToRefineTypes when Flag.Method.(not !relative_complete && not !no_exparam) ->
      Verbose.printf "@.REFINEMENT FAILED!@.";
      Verbose.printf "Restart with relative_complete := true@.@.";
      Flag.Method.relative_complete := true;
      run_cegar prog
  | Fpat.RefTypInfer.FailedToRefineExtraParameters ->
      Fpat.RefTypInfer.params := [];
      Fpat.RefTypInfer.prev_sol := [];
      Fpat.RefTypInfer.prev_constrs := [];
      incr Fpat.RefTypInfer.number_of_extra_params;
      run_cegar prog


let improve_precision e =
  match e with
  | Fpat.RefTypInfer.FailedToRefineTypes when Flag.Method.(not !insert_param_funarg && not !no_exparam) ->
      Flag.Method.insert_param_funarg := true
  | Fpat.RefTypInfer.FailedToRefineTypes when not !Flag.Method.relative_complete && not !Flag.Method.no_exparam ->
      Verbose.printf "@.REFINEMENT FAILED!@.";
      Verbose.printf "Restart with relative_complete := true@.@.";
      Flag.Method.relative_complete := true
  | Fpat.RefTypInfer.FailedToRefineExtraParameters when !Flag.Method.relative_complete && not !Flag.Method.no_exparam ->
      Fpat.RefTypInfer.params := [];
      Fpat.RefTypInfer.prev_sol := [];
      Fpat.RefTypInfer.prev_constrs := [];
      incr Fpat.RefTypInfer.number_of_extra_params
  | _ -> raise e

let print_result_delimiter () =
  if not !!is_only_result then
    Format.printf "@.%s@.@." @@ String.make !!Format.get_margin '='

let trans_env top_funs make_get_rtyp env : (Syntax.id * Ref_type.t) list =
  let get_rtyp f =
    List.assoc f env
    |@> Debug.printf "trans_env %a: %a@." CEGAR_print.var f CEGAR_ref_type.print
  in
  let aux f = Option.try_any (fun () -> f, Ref_type.rename @@ make_get_rtyp get_rtyp f) in
  List.filter_map aux top_funs

let report orig parsed (result, make_get_rtyp, set_main, main) =
  print_result_delimiter ();
  match result with
  | CEGAR.Safe env ->
      Flag.Log.result := "Safe";
      set_status "Done: Safe";
      Debug.printf "report env: %a@." (List.print @@ Pair.print CEGAR_print.var CEGAR_ref_type.print) env;
      let top_funs = Term_util.get_top_funs @@ Problem.term parsed in
      Debug.printf "report top_funs: %a@." (List.print Print.id) top_funs;
      let env' = trans_env top_funs make_get_rtyp env in
      Debug.printf "report env': %a@." (List.print @@ Pair.print Print.id Ref_type.print) env';
      if Flag.Method.(!mode = FairTermination) => !!Verbose.check then
        if !Flag.Print.result then
          report_safe env' orig parsed
  | CEGAR.Unsafe(sol,_) ->
      let s =
        if Flag.Method.(!mode = NonTermination || !ignore_non_termination) then
          "Unknown."
        else if !Flag.use_abst <> [] then
          Format.asprintf "Unknown (because of abstraction options %a)" Print.(list string) !Flag.use_abst
        else
          "Unsafe"
      in
      Flag.Log.result := s;
      set_status ("Done: " ^ s);
      if !Flag.Print.result then
        report_unsafe main sol set_main

let check ?fun_list ?(exparam_sol=[]) spec pp =
  let preprocessed, make_get_rtyp, set_main, main = cegar_of_preprocessed ?fun_list spec pp in
  let cegar_prog =
    if Flag.(Method.(List.mem !mode [FairTermination;Termination]) && !Termination.add_closure_exparam) then
      begin
        Debug.printf "exparam_sol: %a@." (List.print @@ Pair.print Id.print Format.pp_print_int) exparam_sol;
        let exparam_sol' = List.map (Pair.map CEGAR_trans.trans_var CEGAR_syntax.make_int) exparam_sol in
        let prog'' = CEGAR_util.map_body_prog (CEGAR_util.subst_map exparam_sol') preprocessed in
        Debug.printf "MAIN_LOOP: %a@." CEGAR_print.prog preprocessed;
        let info = {preprocessed.CEGAR_syntax.info with CEGAR_syntax.exparam_orig=Some preprocessed} in
        {prog'' with CEGAR_syntax.info}
      end
    else
      preprocessed
  in
  let result = CEGAR.run cegar_prog in
  result, make_get_rtyp, set_main, main


type bin_input =
    {args : string list;
     preprocessed : Preprocess.result list}

let check_parallel ?fun_list ?(exparam_sol=[]) spec pps =
  if !Flag.Print.progress
  then Color.printf Color.Green "Verifying sub-problems in parallel ... @?";
  if exparam_sol <> [] then unsupported "check_parallel";
  if spec.Spec.abst_cegar_env <> [] then unsupported "check_parallel";
  let problem i preprocessed =
    let input = Filename.change_extension !Flag.mainfile @@ Format.sprintf "%d.bin" i in
    let status = Filename.change_extension input "status" in
    let cmd = Format.sprintf "%s -s -limit %d %s" Sys.argv.(0) !Flag.Limit.time_parallel input in
    i, input, status, cmd, preprocessed
  in
  let problems = List.mapi problem pps in
  let prepare (_,input,status,_,preprocessed) =
    let args = !Flag.Log.args in
    let bin = {args; preprocessed} in
    Marshal.to_file ~flag:[Marshal.Closures] input bin;
    IO.empty_file status
  in
  let len = List.length pps in
  List.iter prepare problems;
  let print_status (i,_,status,_,_) =
    let s = BatPervasives.input_file status in
    let f,st =
      try
        let f,st = String.split s ~by:"," in
        float_of_string f, st
      with _ -> -1., s
    in
    if f < 0. then
      Verbose.printf "%d: %-40s@." i st
    else
      let len = 40 in
      let l = int_of_float (0.5 +. f *. float_of_int len) in
      let l' = min l len in
      let s1 = String.make l' '#' in
      let s2 =
        let finished = List.exists (String.starts_with st) ["Done: ";"Error: "] in
        String.make (len - l') (if finished then '#' else ' ')
      in
      Verbose.printf "%d: [%s%s]  %-40s@." i s1 s2 st
  in
  let b = Unix.isatty Unix.stdout in
  let rec wait () =
    let pid,st = Unix.(waitpid [WNOHANG] (-1)) in
    if b then List.iter print_status problems;
    if b then Verbose.printf "%a" Cursor.up_begin len;
    if pid = 0 then
      wait ()
    else
      pid, st
  in
  if b then Verbose.printf "%t" Cursor.hide;
  Unix.parallel ~wait !Flag.Method.parallel @@ List.map (fun (_,_,_,cmd,_) -> cmd) problems;
  if b then Verbose.printf "%t%a" Cursor.show Cursor.down len;
  if !Flag.Print.progress then Color.printf Color.Green "DONE!@.@.";
  exit 0

let rec loop ?make_pps ?fun_list ?exparam_sol spec problem =
  let preprocessed = run_preprocess ?make_pps spec problem in
  if !Flag.Method.parallel >= 2 then
    check_parallel ?fun_list ?exparam_sol spec preprocessed
  else
    try
      List.map (check ?fun_list ?exparam_sol spec) preprocessed
    with e ->
         if !!Debug.check then Printexc.print_backtrace stdout;
         improve_precision e;
         loop ?make_pps ?fun_list ?exparam_sol spec problem

let run ?make_pps ?fun_list ?orig ?exparam_sol ?(spec=Spec.init) parsed =
  let results = loop ?make_pps ?fun_list ?exparam_sol spec parsed in
  let bool_of_result (result, make_get_rtyp, set_main, main) =
    match result with
    | CEGAR.Safe _ -> true
    | CEGAR.Unsafe _ -> false
  in
  if results = [] then
    begin
      Flag.Log.result := "Safe";
      set_status "Done: Safe";
      if Flag.Method.(!mode = FairTermination) => !!Verbose.check then
        if !Flag.Print.result then
          report_safe [] orig parsed
    end;
  List.iter (report orig parsed) results;
  List.for_all bool_of_result results
