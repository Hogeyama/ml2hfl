open Util

let init () =
  Term_util.typ_excep := Type.TData("exn",true)

let rec trans_and_print f desc proj ?(opt=true) ?(pr=Print.term_typ) t =
  let b = false in
  if b then Format.printf "START: %s@." desc;
  let r = f t in
  if b then Format.printf "END: %s@." desc;
  let t' = proj r in
  if !Flag.debug_level > 0 && t <> t' && opt
  then Format.printf "###%a:@. @[%a@.@." Color.s_red desc pr t';
  r

let merge_get_rtyp get_rtyp1 get_rtyp2 f typ = get_rtyp1 f (get_rtyp2 f typ)
let (-||) = merge_get_rtyp


type preprocess =
  | Init
  | Replace_const
  | Encode_mutable_record
  | Abst_ref
  | Make_fun_tuple
  | Make_ext_funs
  | Copy_poly
  | Ignore_non_termination
  | Beta_reduce_trivial
  | Recover_const_attr
  | Decomp_pair_eq
  | Add_preds
  | Replace_fali_with_raise
  | Encode_recdata
  | Replace_base_with_int
  | Encode_list
  | Ret_fun
  | Ref_trans
  | Tupling
  | Inline
  | Cps
  | Remove_pair
  | Replace_bottom_def
  | Add_cps_preds
  | Eliminate_same_arguments
  | Insert_unit_param
  | Preprocessfortermination

let last acc = snd @@ List.hd acc
let last_t acc = fst @@ last acc
let last_get_rtyp acc = snd @@ last acc
let take_result l acc = fst @@ List.assoc l acc

let get_rtyp_id _ typ = typ

let preprocesses t spec =
  [
    Replace_const,
    ("replace_const",
     (fun _ -> !Flag.replace_const),
     (fun acc -> CFA.replace_const @@ last_t acc, get_rtyp_id));
    Encode_mutable_record,
    ("encode_mutable_record",
     Fun.const true,
     (fun acc -> Trans.encode_mutable_record @@ last_t acc, get_rtyp_id));
    Abst_ref,
    ("abst_ref",
     Fun.const true,
     (fun acc -> Trans.abst_ref @@ last_t acc, get_rtyp_id));
    Make_fun_tuple,
    ("make_fun_tuple",
     (fun _ -> !Flag.tupling),
     (fun acc -> Ref_trans.make_fun_tuple @@ last_t acc, get_rtyp_id));
    Make_ext_funs,
    ("make_ext_funs",
     Fun.const true,
     (fun acc -> Trans.make_ext_funs (Spec.get_ext_ref_env spec @@ last_t acc) @@ last_t acc, get_rtyp_id));
    Copy_poly,
    ("copy_poly",
     Fun.const true,
     (fun acc -> Trans.copy_poly_funs @@ last_t acc, get_rtyp_id));
    Ignore_non_termination,
    ("ignore_non_termination",
     (fun _ -> !Flag.ignore_non_termination),
     (fun acc -> Trans.ignore_non_termination @@ last_t acc, get_rtyp_id));
    Beta_reduce_trivial,
    ("beta_reduce_trivial",
     Fun.const true,
     (fun acc -> Trans.beta_reduce_trivial @@ last_t acc, get_rtyp_id));
    Recover_const_attr,
    ("recover_const_attr",
     Fun.const true,
     (fun acc -> Trans.recover_const_attr @@ last_t acc, get_rtyp_id));
    Decomp_pair_eq,
    ("decomp_pair_eq",
     Fun.const true,
     (fun acc -> Trans.decomp_pair_eq @@ last_t acc, get_rtyp_id));
    Add_preds,
    ("add_preds",
     (fun _ -> spec.Spec.abst_env <> []),
     (fun acc -> Trans.replace_typ (Spec.get_abst_env spec @@ last_t acc) @@ last_t acc, get_rtyp_id));
    Replace_fali_with_raise,
    ("replace_fali_with_raise",
     (fun _ -> !Flag.fail_as_exception),
     (fun acc -> Trans.replace_fail_with_raise @@ last_t acc, get_rtyp_id));
    Encode_recdata,
    ("encode_recdata",
     Fun.const true,
     (fun acc -> Encode_rec.trans @@ last_t acc, get_rtyp_id));
    Replace_base_with_int,
    ("replace_base_with_int",
     (fun _ -> !Flag.base_to_int),
     (fun acc -> Trans.replace_base_with_int @@ last_t acc, get_rtyp_id));
    Encode_list,
    ("encode_list",
     Fun.const true,
     (fun acc -> Encode_list.trans @@ last_t acc));
    Ret_fun,
    ("ret_fun",
     (fun acc -> !Flag.tupling),
     (fun acc -> Ret_fun.trans @@ last_t acc));
    Ref_trans,
    ("ref_trans",
     (fun _ -> !Flag.tupling),
     (fun acc -> Ref_trans.trans @@ last_t acc));
    Tupling,
    ("tupling",
     (fun _ -> !Flag.tupling),
     (fun acc -> Tupling.trans @@ last_t acc));
    Inline,
    ("inline",
     Fun.const true,
     (fun acc -> let t = last_t acc in Trans.inlined_f (Spec.get_inlined_f spec t) t, get_rtyp_id));
    Cps,
    ("CPS",
     (fun _ -> !Flag.trans_to_CPS),
     (fun acc -> CPS.trans @@ last_t acc));
    Remove_pair,
    ("remove_pair",
     (fun _ -> !Flag.trans_to_CPS),
     (fun acc -> Curry.remove_pair @@ last_t acc));
    Replace_bottom_def,
    ("replace_bottom_def",
     Fun.const true,
     (fun acc -> Trans.replace_bottom_def @@ last_t acc, get_rtyp_id));
    Add_preds,
    ("add_preds",
     (fun _ -> spec.Spec.abst_cps_env <> []),
     (fun acc -> Trans.replace_typ (Spec.get_abst_cps_env spec @@ last_t acc) @@ last_t acc, get_rtyp_id));
    Eliminate_same_arguments,
    ("eliminate same arguments",
     (fun _ -> !Flag.elim_same_arg),
     (fun acc -> Elim_same_arg.trans @@ last_t acc, get_rtyp_id));
    Insert_unit_param,
    ("insert unit param",
     (fun _ -> !Flag.insert_param_funarg),
     (fun acc -> Trans.insert_param_funarg @@ last_t acc, get_rtyp_id));
    Preprocessfortermination,
    ("preprocessForTermination",
     (fun _ -> !Flag.mode = Flag.Termination),
     (fun acc -> !BRA_types.preprocessForTerminationVerification @@ last_t acc, get_rtyp_id));
  ]


let preprocess t spec =
  let results =
    let pps = preprocesses t spec in
    let aux acc (label,(name,cond,f)) =
      if cond acc then
        let t, get_rtyp = f acc in
        let get_rtyp' = last_get_rtyp acc -|| get_rtyp in
        (label, (t, get_rtyp'))::acc
      else
        acc
    in
    List.fold_left aux [Init, (t, fun _ typ -> typ)] pps
  in
  let t,get_rtyp = last results in
  let fun_list = Term_util.get_top_funs @@ take_result Decomp_pair_eq results in

  if !Flag.exp2 then
    begin
      let oc = open_out (Filename.change_extension !Flag.filename "pml") in
      let ocf = Format.formatter_of_out_channel oc in
      Format.fprintf ocf "%a@." Print.term_typ t;
      close_out oc
    end;

  let prog,map,rmap,get_rtyp_trans = CEGAR_trans.trans_prog (*~spec:abst_cegar_env*) t in
  let abst_cegar_env = Spec.get_abst_cegar_env spec prog |@(not !Flag.only_result)&> Spec.print_abst_cegar_env Format.std_formatter in
  let prog = CEGAR_trans.add_env abst_cegar_env prog in
  let get_rtyp' = get_rtyp -|| get_rtyp_trans in

  let info =
    let orig_fun_list =
      let aux x = List.assoc_option (CEGAR_trans.trans_var x) map in
      List.filter_map aux fun_list
    in
    let inlined = List.map CEGAR_trans.trans_var spec.Spec.inlined in
    let fairness =
      if !Flag.mode = Flag.FairNonTermination then
        Some spec.Spec.fairness
      else
        None
    in
    {CEGAR_syntax.orig_fun_list; CEGAR_syntax.inlined; CEGAR_syntax.fairness}
  in
  prog, rmap, get_rtyp', info



let report_safe env rmap get_rtyp orig t0 =
  if Flag.print_ref_typ_debug
  then
    begin
      Format.printf "Refinement types:@.";
      List.iter (fun (f,typ) -> Format.printf "  %s: %a@." f CEGAR_ref_type.print typ) env;
      Format.printf "@."
    end;
  let env' =
    if !Flag.insert_param_funarg
    then []
    else
      let aux (f,rtyp) : (Syntax.id * Ref_type.t) list =
        try
          let f' = List.assoc f rmap in
          [f', Ref_type.rename @@ get_rtyp f' rtyp]
        with
        | Not_found -> []
        | _ ->
            if not !Flag.tupling then Format.printf "Some refinement types cannot be shown (unimplemented)@.@.";
            []
      in
      if !Flag.relative_complete then
        let _ = Flag.web := true in
        let res = List.rev_map_flatten aux env in
        let _ = Flag.web := false in
        res
      else
        List.rev_map_flatten aux env
  in
  if !Flag.write_annot
  then
    env'
    |> List.map (fun (id, typ) -> Id.name id, typ)
    |> WriteAnnot.f !Flag.filename orig;
  let only_result_termination = !Flag.debug_level <= 0 && !Flag.mode = Flag.Termination in
  begin
    match !Flag.mode with
    | Flag.NonTermination ->
        Color.printf Color.Bright "Non-terminating!";
        Format.printf "@.@."
    | Flag.FairNonTermination ->
        Color.printf Color.Bright "Fair Infinite Execution found!";
        Format.printf "@.@."
    | Flag.Termination when !Flag.debug_level <= 0 ->
        Color.printf Color.Bright "Safe!";
        Format.printf "@.@."
    | _ ->
        Color.printf Color.Bright "Safe!";
        Format.printf "@.@."
  end;
  if !Flag.relative_complete then
    begin
      let map =
        List.map
          (fun (x, t) ->
           Id.make (-1) (Fpat.Idnt.string_of x) Type.TInt,
           CEGAR_trans.trans_inv_term @@ FpatInterface.inv_term @@ t)
          !Fpat.RefTypInfer.prev_sol
      in
      let t = Term_util.subst_map map t0 in
      Format.printf "Program with Quantifiers Added:@.";
      Flag.web := true;
      Format.printf "  @[<v>%a@]@.@." Print.term t;
      Flag.web := false
    end;
  if env' <> [] && not only_result_termination then
    begin
      Format.printf "Refinement Types:@.";
      let env' = List.map (Pair.map_snd Ref_type.simplify) env' in
      let pr (f,typ) = Format.printf "  %s: %a@." (Id.name f) Ref_type.print typ in
      List.iter pr env';
      Format.printf "@.";

      if !Flag.print_abst_typ && env' <> [] && not only_result_termination then
        begin
          Format.printf "Abstraction Types:@.";
          let pr (f,typ) = Format.printf "  %s: %a@." (Id.name f) Print.typ @@ Ref_type.to_abst_typ typ in
          List.iter pr env';
          Format.printf "@."
        end
      end


let report_unsafe main ce set_target =
  let s =
    if !Flag.mode = Flag.NonTermination || !Flag.ignore_non_termination then
      "Unknown."
    else
      "Unsafe!"
  in
  Color.printf Color.Bright "%s@.@." s;
  Option.may (fun (main_fun, arg_num) ->
              Format.printf "Input for %s:@.  %a@." main_fun
                            (print_list Format.pp_print_int "; ") (List.take arg_num ce)) main;
  try
    Format.printf "@[<v 2>Error trace:%a@." Eval.print (ce,set_target)
  with Unsupported s -> Format.printf "@.Unsupported: %s@.@." s


(* TODO: merge with "run" *)
let rec run_cegar prog =
  init ();
  try
    match CEGAR.run prog CEGAR_syntax.empty_cegar_info with
    | _, CEGAR.Safe env ->
        Flag.result := "Safe";
        Color.printf Color.Bright "Safe!@.@.";
        true
    | _, CEGAR.Unsafe ce ->
        Flag.result := "Unsafe";
        Color.printf Color.Bright "Unsafe!@.@.";
        false
  with
  | Fpat.RefTypInfer.FailedToRefineTypes when not !Flag.insert_param_funarg && not !Flag.no_exparam ->
      Flag.insert_param_funarg := true;
      run_cegar prog
  | Fpat.RefTypInfer.FailedToRefineTypes when not !Flag.relative_complete && not !Flag.no_exparam ->
      if not !Flag.only_result then Format.printf "@.REFINEMENT FAILED!@.";
      if not !Flag.only_result then Format.printf "Restart with relative_complete := true@.@.";
      Flag.relative_complete := true;
      run_cegar prog
  | Fpat.RefTypInfer.FailedToRefineExtraParameters ->
      Fpat.RefTypInfer.params := [];
      Fpat.RefTypInfer.prev_sol := [];
      Fpat.RefTypInfer.prev_constrs := [];
      incr Fpat.RefTypInfer.number_of_extra_params;
      run_cegar prog


let rec run orig exparam_sol ?(spec=Spec.init) parsed =
  init ();
  let main,set_target =
    if spec.Spec.ref_env = [] then
      trans_and_print Trans.set_main "set_main" snd parsed
    else
      let ref_env = Spec.get_ref_env spec parsed |@ not !Flag.only_result &> Spec.print_ref_env Format.std_formatter in
      None, trans_and_print (Trans.ref_to_assert ref_env) "ref_to_assert" Fun.id parsed
  in
  (** Unno: I temporally placed the following code here
            so that we can infer refinement types for a safe program
            with extra parameters added *)
  let t0 =
    if !Flag.relative_complete then
      let t = Trans.lift_fst_snd set_target in
      let t = FpatInterface.insert_extra_param t in (* THERE IS A BUG in exception handling *)
      if true && !Flag.debug_level > 0 then
        Format.printf "insert_extra_param (%d added)::@. @[%a@.@.%a@.@."
                      (List.length !Fpat.RefTypInfer.params) Print.term t Print.term' t;
      t
    else
      set_target
  in
  (**)
  let prog, rmap, get_rtyp, info = preprocess t0 spec in
  let prog' =
    if !Flag.mode = Flag.FairTermination && !Flag.add_closure_exparam
    then
      let () = if false then Format.printf "%a@." (List.print @@ Pair.print Id.print Format.pp_print_int) exparam_sol in
      let exparam_sol' = List.map (Pair.map CEGAR_trans.trans_var CEGAR_syntax.make_int) exparam_sol in
      let prog'' = CEGAR_util.map_body_prog (CEGAR_util.subst_map exparam_sol') prog in
      if false then Format.printf "MAIN_LOOP: %a@." CEGAR_print.prog prog;
      {prog'' with CEGAR_syntax.info={prog.CEGAR_syntax.info with CEGAR_syntax.exparam_orig=Some prog}}
    else
      prog
  in
  if !Flag.trans_to_CPS then FpatInterface.init prog';
  try
    match CEGAR.run prog' info with
    | _, CEGAR.Safe env ->
        Flag.result := "Safe";
        if not !Flag.exp && (!Flag.mode = Flag.FairTermination => (!Flag.debug_level > 0))
        then report_safe env rmap get_rtyp orig t0;
        true
    | _, CEGAR.Unsafe ce ->
        Flag.result := "Unsafe";
        if not !Flag.exp
        then report_unsafe main ce set_target;
        false
  with
  | Fpat.RefTypInfer.FailedToRefineTypes when not !Flag.insert_param_funarg && not !Flag.no_exparam->
      Flag.insert_param_funarg := true;
      run orig exparam_sol ~spec parsed
  | Fpat.RefTypInfer.FailedToRefineTypes when not !Flag.relative_complete && not !Flag.no_exparam ->
      if not !Flag.only_result then Format.printf "@.REFINEMENT FAILED!@.";
      if not !Flag.only_result then Format.printf "Restart with relative_complete := true@.@.";
      Flag.relative_complete := true;
      run orig exparam_sol ~spec parsed
  | Fpat.RefTypInfer.FailedToRefineExtraParameters when !Flag.relative_complete && not !Flag.no_exparam ->
      Fpat.RefTypInfer.params := [];
      Fpat.RefTypInfer.prev_sol := [];
      Fpat.RefTypInfer.prev_constrs := [];
      incr Fpat.RefTypInfer.number_of_extra_params;
      run orig exparam_sol ~spec parsed
