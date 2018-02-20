open Util

type result = Safe of (Syntax.id * Ref_type.t) list | Unsafe of int list

module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)



let preprocess make_pps ?(fun_list=None) t spec =
  let pps' =
    match make_pps with
    | None -> Preprocess.all spec
    | Some make_pps' -> make_pps' spec
  in
  let results = Preprocess.run pps' t in
  let t = Preprocess.last_t results in
  let fun_list' =
    match fun_list with
    | None -> Term_util.get_top_funs Preprocess.(take_result Decomp_pair_eq results)
    | Some fun_list' -> fun_list'
  in

  let prog,map,_,make_get_rtyp_trans = CEGAR_trans.trans_prog (*~spec:abst_cegar_env*) t in
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
  CEGAR_syntax.{prog with info}, make_get_rtyp



let write_annot env orig =
  env
  |> List.map (Pair.map_fst Id.name)
  |> WriteAnnot.f !!Flag.mainfile orig

let report_safe env orig t0 =
  if !Flag.PrettyPrinter.write_annot && List.length !Flag.filenames = 1 then write_annot env orig;

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
      Format.printf "Program with Quantifiers Added:@.";
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


let report_unsafe main ce set_target =
  let s =
    if Flag.Method.(!mode = NonTermination || !ignore_non_termination) then
      "Unknown."
    else
      if !Flag.use_abst then
        "Unknown (because of abstraction options)"
      else
        "Unsafe!"
  in
  Color.printf Color.Bright "%s@.@." s;
  if not !Flag.use_abst then
    let pr (main_fun, arg_num) =
      if arg_num > 0 then
        Format.printf "Input for %s:@.  %a@." main_fun (print_list Format.pp_print_int "; ") (List.take arg_num ce)
    in
    Option.may pr main;
    try
      Format.printf "@[<v 2>Error trace:%a@." Eval.print (ce,set_target)
    with Unsupported s -> Format.printf "@.Unsupported: %s@.@." s


let rec run_cegar prog =
  try
    match CEGAR.run prog with
    | CEGAR.Safe env ->
        Flag.Log.result := "Safe";
        Color.printf Color.Bright "Safe!@.@.";
        true
    | CEGAR.Unsafe _ ->
        Flag.Log.result := "Unsafe";
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


let insert_extra_param t =
  let t' =
    t
    |> Trans.lift_fst_snd
    |> FpatInterface.insert_extra_param (* THERE IS A BUG in exception handling *)
  in
  if true then
    Verbose.printf "insert_extra_param (%d added)::@. @[%a@.@.%a@.@."
                  (List.length !Fpat.RefTypInfer.params) Print.term t' Print.term' t';
  t'

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

let rec loop make_pps ?(fun_list=None) exparam_sol ?(spec=Spec.init) parsed set_target =
  (** Unno: I temporally placed the following code here
            so that we can infer refinement types for a safe program
            with extra parameters added *)
  let set_target' =
    if !Flag.Method.relative_complete then
      insert_extra_param set_target
    else
      set_target
  in
  (**)
  let prog, make_get_rtyp = preprocess make_pps ~fun_list set_target' spec in
  let prog' =
    if Flag.(Method.(!mode = FairTermination) && !Termination.add_closure_exparam)
    then
      let () = if false then Format.printf "%a@." (List.print @@ Pair.print Id.print Format.pp_print_int) exparam_sol in
      let exparam_sol' = List.map (Pair.map CEGAR_trans.trans_var CEGAR_syntax.make_int) exparam_sol in
      let prog'' = CEGAR_util.map_body_prog (CEGAR_util.subst_map exparam_sol') prog in
      if false then Format.printf "MAIN_LOOP: %a@." CEGAR_print.prog prog;
      let info = {prog.CEGAR_syntax.info with CEGAR_syntax.exparam_orig=Some prog} in
      {prog'' with CEGAR_syntax.info}
    else
      prog
  in
  try
    let result = CEGAR.run prog' in
    result, make_get_rtyp, set_target'
  with e ->
    if !!Debug.check then Printexc.print_backtrace stdout;
    improve_precision e;
    loop make_pps ~fun_list exparam_sol ~spec parsed set_target


let trans_env top_funs make_get_rtyp env : (Syntax.id * Ref_type.t) list =
  let get_rtyp f = List.assoc f env in
  let aux f = Option.try_any (fun _ -> f, Ref_type.rename @@ make_get_rtyp get_rtyp f) in
  List.filter_map aux top_funs

let verify ?(make_pps=None) ?(fun_list=None) exparam_sol spec parsed =
  let main,set_target =
    if spec.Spec.ref_env = [] then
      Preprocess.trans_and_print Trans.set_main "set_main" Fun.id snd parsed
    else
      let ref_env =
        Spec.get_ref_env spec parsed
        |@> Verbose.printf "%a@." Spec.print_ref_env
        |> Ref_type.Env.of_list
      in
      None, Preprocess.trans_and_print (Trans.ref_to_assert ref_env) "ref_to_assert" Fun.id Fun.id parsed
  in
  loop make_pps ~fun_list exparam_sol ~spec parsed set_target, main, set_target

let print_result_delimiter () =
  if not !!is_only_result then
    Format.printf "@.%s@.@." @@ String.make !!Format.get_margin '='

let run ?make_pps ?fun_list orig ?(exparam_sol=[]) ?(spec=Spec.init) parsed =
  let (result, make_get_rtyp, set_target'), main, set_target = verify ~make_pps ~fun_list exparam_sol spec parsed in
  print_result_delimiter ();
  match result with
  | CEGAR.Safe env ->
      Flag.Log.result := "Safe";
      let env' = trans_env (Term_util.get_top_funs parsed) make_get_rtyp env in
      if Flag.Method.(!mode = FairTermination) => !!Verbose.check then
        if !Flag.Print.result then
          report_safe env' orig set_target';
      true
  | CEGAR.Unsafe(sol,_) ->
      Flag.Log.result := "Unsafe";
      if !Flag.Print.result then
        report_unsafe main sol set_target;
      false
