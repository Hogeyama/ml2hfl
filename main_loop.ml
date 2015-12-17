open Util

type result = Safe of (Syntax.id * Ref_type.t) list | Unsafe of int list

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

let preprocess t spec =
  let fun_list,t,get_rtyp =
    if !Flag.init_trans
    then
      let t = t |&!Flag.replace_const&> trans_and_print CFA.replace_const "replace_const" Fun.id in
      let t = trans_and_print Trans.encode_mutable_record "encode_mutable_record" Fun.id t in
      let t = trans_and_print Trans.abst_ref "abst_ref" Fun.id t in
      let t = t |&!Flag.tupling&> trans_and_print Ref_trans.make_fun_tuple "make_fun_tuple" Fun.id in
      let ext_ref_env = Spec.get_ext_ref_env spec t |@not!Flag.only_result&> Spec.print_ext_ref_env Format.std_formatter in
      let t = trans_and_print (Trans.make_ext_funs ext_ref_env) "make_ext_funs" Fun.id t in
      let t = trans_and_print Trans.copy_poly_funs "copy_poly" Fun.id t in
      let t = t |&!Flag.ignore_non_termination&> trans_and_print Trans.ignore_non_termination "ignore_non_termination" Fun.id in
      let t = trans_and_print Trans.beta_reduce_trivial "beta_reduce_trivial" Fun.id t in
      let t = trans_and_print Trans.recover_const_attr "recover_const_attr" Fun.id t in
      let t = trans_and_print Trans.decomp_pair_eq "decomp_pair_eq" Fun.id t in
      let fun_list = Term_util.get_top_funs t in
      let abst_env = Spec.get_abst_env spec t |@not!Flag.only_result&> Spec.print_abst_env Format.std_formatter in
      let t = trans_and_print (Trans.replace_typ abst_env) "add_preds" Fun.id ~pr:Print.term' ~opt:(spec.Spec.abst_env<>[]) t in
      let t = t |&!Flag.fail_as_exception&> trans_and_print Trans.replace_fail_with_raise "replace_fali_with_raise" Fun.id in
      let t = trans_and_print Encode_rec.trans "encode_recdata" Fun.id t in
      let t = t |&!Flag.base_to_int&> trans_and_print Trans.replace_base_with_int "replace_base_with_int" Fun.id in
      let t,get_rtyp_list = trans_and_print Encode_list.trans "encode_list" fst t in
      let get_rtyp = get_rtyp_list in
      let t,get_rtyp =
        if !Flag.tupling
        then
          let t,get_rtyp_ret_fun = trans_and_print Ret_fun.trans "ret_fun" fst t in
          let t,get_rtyp_ref_trans = trans_and_print Ref_trans.trans "ref_trans" fst t in
          let t,get_rtyp_tupling = trans_and_print Tupling.trans "tupling" fst t in
          t, get_rtyp -|| get_rtyp_ret_fun -|| get_rtyp_ref_trans -|| get_rtyp_tupling
        else
          t, get_rtyp
      in
      let inlined_f = Spec.get_inlined_f spec t in
      let t = trans_and_print (Trans.inlined_f inlined_f) "inline" Fun.id t in
      let t,get_rtyp =
        if !Flag.trans_to_CPS
        then
          let t,get_rtyp_cps_trans = trans_and_print CPS.trans "CPS" fst ~pr:Print.term t in
          let get_rtyp = get_rtyp -|| get_rtyp_cps_trans in
          let t,get_rtyp_remove_pair = trans_and_print Curry.remove_pair "remove_pair" fst ~pr:Print.term t in
          let get_rtyp = get_rtyp -|| get_rtyp_remove_pair in
          t, get_rtyp
        else
          t, get_rtyp
      in
      let t = trans_and_print Trans.replace_bottom_def "replace_bottom_def" Fun.id ~pr:Print.term t in
      let abst_cps_env = Spec.get_abst_cps_env spec t |@not!Flag.only_result&> Spec.print_abst_cps_env Format.std_formatter in
      let t = trans_and_print (Trans.replace_typ abst_cps_env) "add_preds" Fun.id ~opt:(spec.Spec.abst_cps_env<>[]) t in
      let t = t |&!Flag.elim_same_arg&> trans_and_print Elim_same_arg.trans "eliminate same arguments" Fun.id in
      let t = t |&!Flag.insert_param_funarg&> trans_and_print Trans.insert_param_funarg "insert unit param" Fun.id in

      (* preprocess for termination mode *)
      let t =
        match !Flag.mode with
        | Flag.Termination -> !BRA_types.preprocessForTerminationVerification t
        | _ -> t
      in

      fun_list, t, get_rtyp
    else
      Term_util.get_top_funs t, t, fun _ typ -> typ
  in

  if !Flag.exp2 then
    begin
      let oc = open_out @@ Filename.change_extension !Flag.filename "pml" in
      let ocf = Format.formatter_of_out_channel oc in
      Format.fprintf ocf "%a@." Print.term_typ t;
      close_out oc
    end;

  (* ill-formed program *)
  let _progWithExparam =
    if !Flag.add_closure_exparam
    then Some (Quadruple.fst @@ CEGAR_trans.trans_prog !ExtraParamInfer.withExparam)
    else None
  in
  (**********************)

  let abst_cegar_env = Spec.get_abst_cegar_env spec t |@(not !Flag.only_result)&> Spec.print_abst_cegar_env Format.std_formatter in
  let prog,map,rmap,get_rtyp_trans = CEGAR_trans.trans_prog ~spec:abst_cegar_env t in
  let get_rtyp = get_rtyp -|| get_rtyp_trans in
   (*
    if !Flag.debug_level > 0 then Format.printf "[before]***************@.    %a@." (CEGAR_util.print_prog_typ' [] []) !Refine.progWithExparam;
    if !Flag.debug_level > 0 then Format.printf "[after]***************@.    %a@." (CEGAR_util.print_prog_typ' [] []) prog;
  *)

  let info =
    let orig_fun_list =
      let aux x = List.assoc_option (CEGAR_trans.trans_var x) map in
      List.filter_map aux fun_list
    in
    let inlined = List.map CEGAR_trans.trans_var spec.Spec.inlined in
    {CEGAR_syntax.orig_fun_list; CEGAR_syntax.inlined}
  in
  prog, rmap, get_rtyp, info



let report_safe env orig t0 =
  if !Flag.write_annot
  then
    env
    |> List.map (Pair.map_fst Id.name)
    |> WriteAnnot.f !Flag.filename orig;
  begin
    match !Flag.mode with
    | Flag.NonTermination ->
        Color.printf Color.Bright "Non-terminating!";
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
      Ref.tmp_set
        Flag.web true
        (fun () -> Format.printf "  @[<v>%a@]@.@." Print.term t)
    end;
  let only_result_termination = !Flag.debug_level <= 0 && !Flag.mode = Flag.Termination in
  if env <> [] && not only_result_termination then
    begin
      Format.printf "Refinement Types:@.";
      let env' = List.map (Pair.map_snd Ref_type.simplify) env in
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
  if !Flag.mode = Flag.NonTermination || !Flag.ignore_non_termination
  then
    Color.printf Color.Bright "Unknown."
  else
    Color.printf Color.Bright "Unsafe!";
  Format.printf "@.@.";
  Option.may (fun (main_fun, arg_num) ->
              Format.printf "Input for %s:@.  %a@." main_fun
                            (print_list Format.pp_print_int "; ") (List.take arg_num ce)) main;
  try
    Format.printf "@[<v 2>Error trace:%a@." Eval.print (ce,set_target)
  with Unsupported s -> Format.printf "@.Unsupported: %s@.@." s


let rec run_cegar prog =
  try
    match CEGAR.run prog CEGAR_syntax.empty_cegar_info with
    | CEGAR.Safe env ->
        Flag.result := "Safe";
        Color.printf Color.Bright "Safe!@.@.";
        true
    | CEGAR.Unsafe ce ->
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


let init_typ_excep () =
  Term_util.typ_excep := Type.TData("exn",true)

let insert_extra_param t =
  let t' =
    t
    |> Trans.lift_fst_snd
    |> FpatInterface.insert_extra_param (* THERE IS A BUG in exception handling *)
  in
  if true && !Flag.debug_level > 0 then
    Format.printf "insert_extra_param (%d added)::@. @[%a@.@.%a@.@."
                  (List.length !Fpat.RefTypInfer.params) Print.term t' Print.term' t';
  t'

let improve_precision e =
  match e with
  | Fpat.RefTypInfer.FailedToRefineTypes when not !Flag.insert_param_funarg && not !Flag.no_exparam->
      Flag.insert_param_funarg := true
  | Fpat.RefTypInfer.FailedToRefineTypes when not !Flag.relative_complete && not !Flag.no_exparam ->
      if not !Flag.only_result then Format.printf "@.REFINEMENT FAILED!@.";
      if not !Flag.only_result then Format.printf "Restart with relative_complete := true@.@.";
      Flag.relative_complete := true
  | Fpat.RefTypInfer.FailedToRefineExtraParameters when !Flag.relative_complete && not !Flag.no_exparam ->
      Fpat.RefTypInfer.params := [];
      Fpat.RefTypInfer.prev_sol := [];
      Fpat.RefTypInfer.prev_constrs := [];
      incr Fpat.RefTypInfer.number_of_extra_params
  | _ -> raise e

let rec loop exparam_sol ?(spec=Spec.init) parsed set_target =
  init_typ_excep ();
  (** Unno: I temporally placed the following code here
            so that we can infer refinement types for a safe program
            with extra parameters added *)
  let set_target' =
    if !Flag.relative_complete then
      insert_extra_param set_target
    else
      set_target
  in
  (**)
  let prog, rmap, get_rtyp, info = preprocess set_target' spec in
  let prog' =
    if !Flag.mode = Flag.FairTermination && !Flag.add_closure_exparam
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
  if !Flag.trans_to_CPS then FpatInterface.init prog';
  try
    let result = CEGAR.run prog' info in
    result, rmap, get_rtyp, set_target'
  with e ->
       improve_precision e;
       loop exparam_sol ~spec parsed set_target


let trans_env rmap get_rtyp env : (Syntax.id * Ref_type.t) list =
  if !Flag.insert_param_funarg
  then []
  else
    let aux (f,rtyp) =
      try
        let f' = List.assoc f rmap in
        Some (f', Ref_type.rename @@ get_rtyp f' rtyp)
      with
      | Not_found -> None
      | _ ->
          Format.printf "Some refinement types cannot be shown (unimplemented)@.@.";
          None
    in
    List.filter_map aux env

let verify exparam_sol spec parsed =
  let main,set_target =
    if spec.Spec.ref_env = []
    then trans_and_print Trans.set_main "set_main" snd parsed
    else
      let ref_env = Spec.get_ref_env spec parsed |@ not !Flag.only_result &> Spec.print_ref_env Format.std_formatter in
      None, trans_and_print (Trans.ref_to_assert ref_env) "ref_to_assert" Fun.id parsed
  in
  loop exparam_sol ~spec parsed set_target, main, set_target


let run orig exparam_sol ?(spec=Spec.init) parsed =
  let (result, rmap, get_rtyp, set_target'), main, set_target = verify exparam_sol spec parsed in
  match result with
  | CEGAR.Safe env ->
      Flag.result := "Safe";
      let env' = trans_env rmap get_rtyp env in
      if not !Flag.exp && !Flag.mode = Flag.FairTermination => (!Flag.debug_level > 0)
      then report_safe env' orig set_target';
      true
  | CEGAR.Unsafe ce ->
      Flag.result := "Unsafe";
      if not !Flag.exp
      then report_unsafe main ce set_target;
      false
