open Util
open Mochi_util
open Main_loop_util

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

let cegar_of_preprocessed ?fun_list spec results =
  let set_main = Option.map fst @@ List.assoc_option Preprocess.Set_main results in
  let main = Option.(set_main >>= return-|Problem.term >>= Trans.get_set_main)  in
  let problem = Preprocess.last_problem results in
  let fun_list' =
    match fun_list with
    | None ->
        begin
          try
            let t = Problem.term Preprocess.(take_result Decomp_pair_eq results) in
            Term_util.get_top_funs t
          with Not_found -> []
        end
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
  CEGAR_syntax.{prog with info}, make_get_rtyp, set_main, main, problem.Problem.info


let run_preprocess ?make_pps spec problem =
  let pps' =
    match make_pps with
    | None -> Preprocess.all spec
    | Some make_pps' -> make_pps' spec
  in
  Preprocess.run_problem pps' problem


let write_annot env orig =
  env
  |> List.map (Pair.map_fst Id.name)
  |> WriteAnnot.f !!Flag.Input.main orig

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
  Color.printf Color.Bright "%s@.@." (Flag.Log.string_of_result false);
  if !Flag.Encode.used_abst = [] then
    let pr main_fun =
      let arg_num = Type.arity @@ Id.typ main_fun in
      if arg_num > 0 then
        Format.printf "Input for %a:@.  %a@." Id.print main_fun (print_list Format.pp_print_int "; ") (List.take arg_num ce)
    in
    Option.may pr main;
    match set_main with
    | None -> ()
    | Some set_main ->
        if not !Flag.Method.slice then
          Format.printf "@[<v 2>Error trace:%a@." Eval.print (ce,set_main)

(** TODO: merge with report_unsafe *)
let report_unsafe_par main ce set_main =
  if !Flag.Encode.used_abst = [] then
    begin
      Color.printf Color.Bright "Unsafe@.@.";
      report_unsafe main ce set_main
    end
  else
    Color.printf Color.Bright "Unknown@.@.";
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
        set_status Flag.Log.Safe;
        Color.printf Color.Bright "Safe!@.@.";
        true
    | CEGAR.Unsafe _ ->
        set_status Flag.Log.Unsafe;
        Color.printf Color.Bright "Unsafe!@.@.";
        false
    | CEGAR.Unknown _ -> unsupported "Main_loop.run_cegar"
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

let status_of_result r =
  match r with
  | CEGAR.Safe env -> Flag.Log.Safe
  | CEGAR.Unsafe _ when Flag.Method.(!mode = NonTermination || !ignore_non_termination) ->
      Flag.Log.Unknown ""
  | CEGAR.Unsafe _ when !Flag.Encode.used_abst <> [] ->
      Flag.Log.Unknown (Format.asprintf "because of abstraction options %a" Print.(list string) !Flag.Encode.used_abst)
  | CEGAR.Unsafe _ ->
      Flag.Log.Unsafe
  | CEGAR.Unknown s when String.starts_with s "Error: " ->
      Flag.Log.Error (snd @@ String.split_nth s (String.length "Error: "))
  | CEGAR.Unknown s ->
      Flag.Log.Unknown s

let report orig parsed num {result; stats; make_get_rtyp; set_main; main; info} =
  print_result_delimiter ();
  if num > 1 then Format.printf "Sub-problem:@.";
  List.iter (Format.printf "  %s@.") info;
  if info <> [] then Format.printf "@.";
  begin
    match result with
    | CEGAR.Safe env ->
        Debug.printf "report env: %a@." Print.(list (CEGAR_print.var * CEGAR_ref_type.print)) env;
        let top_funs = Term_util.get_top_funs @@ Problem.term parsed in
        Debug.printf "report top_funs: %a@." Print.(list id) top_funs;
        let env' = trans_env top_funs make_get_rtyp env in
        Debug.printf "report env': %a@." Print.(list (id * Ref_type.print)) env';
        if Flag.Method.(!mode = FairTermination) => !!Verbose.check then
          if !Flag.Print.result then
            report_safe env' orig parsed
    | CEGAR.Unsafe(sol,_) ->
        if !Flag.Print.result then
          report_unsafe main sol set_main
    | CEGAR.Unknown s when String.starts_with s "Error: " ->
        Color.printf Color.Bright "%s@.@." s
    | CEGAR.Unknown s ->
        Color.printf Color.Bright "Unknown";
        if s <> "" then Color.printf Color.Bright " %s" s;
        Color.printf Color.Bright "@.@."
  end;
  if num > 1 then
    match stats with
    | None -> ()
    | Some {cycles; total; abst; mc; refine} ->
        Format.printf "CEGAR-cycles: %d@." cycles;
        Format.printf "total: %.3f sec@." total;
        Format.printf "  abst: %.3f sec@." abst;
        Format.printf "  mc: %.3f sec@." mc;
        Format.printf "  refine: %.3f sec@." refine


let check ?fun_list ?(exparam_sol=[]) spec pp =
  let preprocessed, make_get_rtyp, set_main, main, info = cegar_of_preprocessed ?fun_list spec pp in
  let cegar_prog =
    if Flag.(Method.(List.mem !mode [FairTermination;Termination]) && !Termination.add_closure_exparam) then
      begin
        Debug.printf "exparam_sol: %a@." Print.(list (id * Format.pp_print_int)) exparam_sol;
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
  {result; stats=None; make_get_rtyp; set_main; main; info}


exception CheckTimeOut

let rec loop ?make_pps ?fun_list ?exparam_sol spec problem =
  let preprocessed = run_preprocess ?make_pps spec problem in
  if !Flag.Parallel.num > 1 then
    if Preprocess.exists_or preprocessed then
      unsupported "parallel check for 'or'"
    else
      let pps = Preprocess.lists_of_paths preprocessed in
      Parallel.check ?fun_list ?exparam_sol spec pps
  else
    let is_singleton = Exception.not_raise Preprocess.get preprocessed in
    try
      let rec aux label acc r =
        match r with
        | Preprocess.Before p ->
            if not is_singleton then Verbose.printf "Start checking sub-problem.@.";
            begin
              try
                Timer.set_handler (fun _ -> raise CheckTimeOut);
                Timer.set @@ float !Flag.Limit.time_subproblem;
                let r = [check ?fun_list ?exparam_sol spec ((label,p)::acc)] in
                Timer.reset ();
                r
              with CheckTimeOut ->
                Verbose.printf "@.TIMEOUT: sub-problem@.@.";
                [return_of_timeout]
            end
        | Preprocess.After {Preprocess.label; problem; op; result} ->
            let acc' = (label,problem)::acc in
            match op with
            | Preprocess.And ->
                List.flatten_map (aux label acc') result
            | Preprocess.Or ->
                let rec aux' acc rest =
                  match rest with
                  | [] -> acc
                  | x::rest' ->
                      let rs = aux label acc' x in
                      match List.find (fun r -> match r.result with CEGAR.Safe _ -> true | _ -> false) rs with
                      | r -> [r]
                      | exception Not_found ->
                          let unknown = List.filter (fun {result} -> match result with CEGAR.Unknown _ -> true | _ -> false) rs in
                          let acc' =
                          if unknown = [] then
                            rs @ acc
                          else
                            unknown @ acc
                          in
                          aux' acc' rest'
                in
                aux' [] result
      in
      aux Preprocess.Init [] preprocessed
    with e ->
         if !!Debug.check then Printexc.print_backtrace stdout;
         improve_precision e;
         loop ?make_pps ?fun_list ?exparam_sol spec problem

let merge_result b r1 r2 =
  match r1, r2 with
  | CEGAR.Safe _, r
  | r, CEGAR.Safe _ -> r
  | CEGAR.Unsafe _ as r, _
  | _, (CEGAR.Unsafe _ as r) -> r
  | CEGAR.Unknown s1, CEGAR.Unknown s2 when b -> CEGAR.Unknown (Format.sprintf "%s, %s" s1 s2)
  | CEGAR.Unknown _, CEGAR.Unknown _ -> CEGAR.Unknown ""

let run ?make_pps ?fun_list ?orig ?exparam_sol ?(spec=Spec.init) parsed =
  let results = loop ?make_pps ?fun_list ?exparam_sol spec parsed in
  let bool_of_result {result} =
    match result with
    | CEGAR.Safe _ -> true
    | CEGAR.Unsafe _ -> false
    | CEGAR.Unknown _ -> false
  in
  let result =
    if results = [] then
      begin
        set_status Flag.Log.Safe;
        if Flag.Method.(!mode = FairTermination) => !!Verbose.check then
          if !Flag.Print.result then
            report_safe [] orig parsed;
        CEGAR.Safe []
      end
    else
      let r = List.reduce (merge_result false) @@ List.map (fun r -> r.result) results in
      set_status @@ status_of_result r;
      r
  in
  let num = List.length results in
  if !Flag.Print.result then
    begin
      List.iter (report orig parsed num) results;
      if num > 1 && !Flag.Parallel.num > 1 then
        report orig parsed (List.length results) {(List.hd results) with result; stats=None; info=[]};
    end;
  List.for_all bool_of_result results
