open Util

let init () =
  Term_util.typ_excep := Type.TConstr("exn",true)

let id x = x

let trans_and_print f desc proj ?(opt=true) ?(pr=Syntax.pp_print_term_typ) t =
  let r = f t in
  let t' = proj r in
  if !Flag.debug_level > 0 && t <> t' && opt
  then Format.printf "%a:@. @[%a@.@." Color.s_red desc pr t';
  r

let merge_get_rtyp get_rtyp1 get_rtyp2 f typ = get_rtyp1 f (get_rtyp2 f typ)

let preprocess t spec =
  let fun_list,t,get_rtyp =
    if !Flag.init_trans
    then
      let t = t |& !Flag.tupling &> trans_and_print Ref_trans.make_fun_tuple "make_fun_tuple" id in
      let t = trans_and_print Trans.make_ext_funs "make_ext_funs" id t in
      let t = trans_and_print Trans.copy_poly_funs "copy_poly" id t in
      let t = trans_and_print Trans.decomp_pair_eq "decomp_pair_eq" id t in
      let fun_list = Term_util.get_top_funs t in
      let spec' = Spec.rename spec t in
      if not !Flag.only_result then Spec.print spec';
      let t = trans_and_print (Trans.replace_typ spec'.Spec.abst_env) "add_preds" id ~opt:(spec<>Spec.init) t in
      let t = trans_and_print Encode_rec.trans "abst_recdata" id t in
      let t,get_rtyp_list = trans_and_print Encode_list.trans "encode_list" fst t in
      let t =
        if !Flag.tupling
        then t
          |> trans_and_print Ret_fun.trans "ret_fun" id
          |> trans_and_print Ref_trans.trans "ref_trans" id
          |> trans_and_print Tupling.trans "tupling" id
        else t
      in
      let get_rtyp = get_rtyp_list in
      let t = trans_and_print (Trans.inlined_f spec'.Spec.inlined_f) "inlined" id t in
      Type_check.check t Type.TUnit;
      let t,get_rtyp_cps_trans = trans_and_print CPS.trans "CPS" fst t in
      let get_rtyp = merge_get_rtyp get_rtyp get_rtyp_cps_trans in
      let t,get_rtyp_remove_pair = trans_and_print Curry.remove_pair "remove_pair" fst t in
      let spec' = Spec.rename spec t in
      if not !Flag.only_result then Spec.print spec';
      let t = trans_and_print (Trans.replace_typ spec'.Spec.abst_cps_env) "add_preds" id ~opt:(spec<>Spec.init) t in
      let t = trans_and_print Elim_same_arg.trans "eliminate same arguments" id t in
      let get_rtyp = merge_get_rtyp get_rtyp get_rtyp_remove_pair in
      let t = t |& !Flag.insert_param_funarg &> trans_and_print Trans.insert_param_funarg "insert unit param" id in

      (* preprocess for termination mode *)
      let t = if !Flag.termination then !BRA_types.preprocessForTerminationVerification t else t in

      fun_list, t, get_rtyp
    else
      let () = Type_check.check t Type.TUnit in
      Term_util.get_top_funs t, t, fun _ typ -> typ
  in

  (* ill-formed program *)
  Refine.progWithExparam := (let p, _, _, _ = CEGAR_trans.trans_prog !ExtraParamInfer.withExparam in p);
  (**********************)

  let spec' = Spec.rename spec t in
  if not !Flag.only_result then Spec.print spec';
  let prog,map,rmap,get_rtyp_trans = CEGAR_trans.trans_prog ~spec:spec'.Spec.abst_cegar_env t in
  let get_rtyp = merge_get_rtyp get_rtyp get_rtyp_trans in

  let info =
    let fun_list =
      let aux x =
        try [List.assoc (CEGAR_trans.trans_var x) map] with Not_found -> []
      in
      rev_flatten_map aux fun_list
    in
    let inlined = List.map CEGAR_trans.trans_var spec.Spec.inlined in
    {CEGAR.orig_fun_list=fun_list; CEGAR.inlined=inlined}
  in
   (*
    if !Flag.debug_level > 0 then Format.printf "[before]***************@.    %a@." (CEGAR_util.print_prog_typ' [] []) !Refine.progWithExparam;
    if !Flag.debug_level > 0 then Format.printf "[after]***************@.    %a@." (CEGAR_util.print_prog_typ' [] []) prog;
  *)
  prog, rmap, get_rtyp, info



let report_safe env rmap get_rtyp orig t0 =
  if Flag.print_ref_typ_debug
  then
    begin
      Format.printf "Refinement types:@.";
      List.iter (fun (f,typ) -> Format.printf "  %s: %a@." f CEGAR_ref_type.print typ) env;
      Format.printf "@."
    end;
  let env' =
    let aux (f,rtyp) : (Syntax.id * Ref_type.t) list =
      try
        let f' = List.assoc f rmap in
        [f', Ref_type.rename (get_rtyp f' rtyp)]
      with
        Not_found -> []
      | _ -> if not !Flag.tupling then Format.printf "Some refinement types cannot be shown (unimplemented)@.@."; []
    in
    if !Flag.insert_param_funarg
    then []
    else
      if !Flag.relative_complete then
        let _ = Flag.web := true in
        let res = rev_map_flatten aux env in
        let _ = Flag.web := false in
        res
      else
        rev_map_flatten aux env
  in
  if !Flag.write_annot
  then
    env' |> List.map (fun (id, typ) -> Id.name id, typ)
         |> WriteAnnot.f !Flag.filename orig;
  let only_result_termination = !Flag.debug_level <= 0 && !Flag.termination in
  if not only_result_termination then (Color.printf Color.Bright "Safe!"; Format.printf "@.@.");
  if !Flag.relative_complete then begin
    let map =
      List.map
        (fun (x, n) ->
          Id.make (-1) (Fpat.Idnt.string_of x) Type.TInt,
          CEGAR_trans.trans_inv_term @@ FpatInterface.inv_term @@ Fpat.IntTerm.make n)
        !Fpat.RefTypeInfer.prev_sol
    in
    let t = Term_util.subst_map map t0 in
    Format.printf "Program with Quantifiers Added:@.";
    Flag.web := true;
    Format.printf "  @[<v>%a@]@.@." Syntax.pp_print_term t;
    Flag.web := false
  end;
  if env' <> [] && not only_result_termination then Format.printf "Refinement Types:@.";
  let env' = List.map (fun (f, typ) -> f, FpatInterface.simplify typ) env' in
  let pr (f,typ) = Format.printf "  %s: %a@." (Id.name f) Ref_type.print typ in
  if not only_result_termination then List.iter pr env';
  if env' <> [] && not only_result_termination then Format.printf "@."


let report_unsafe main_fun arg_num ce set_target =
  Color.printf Color.Bright "Unsafe!";
  Format.printf "@.@.";
  if main_fun <> "" && arg_num <> 0
  then
    Format.printf "Input for %s:@.  %a@." main_fun
      (print_list Format.pp_print_int "; ") (take ce arg_num);
  Format.printf "@[<v 2>Error trace:%a@."  Eval.print (ce,set_target)



let rec run orig parsed =
  init ();
  let t = parsed in
  if false && !Flag.debug_level > 0
  then Format.printf "parsed::@. @[%a@.@." Syntax.pp_print_term' t;
  let spec =
    let spec1 =
      begin
        if !Flag.use_spec && !Flag.spec_file = ""
        then
          try
            let spec = Filename.chop_extension !Flag.filename ^ ".spec" in
            if Sys.file_exists spec then Flag.spec_file := spec
          with Invalid_argument "Filename.chop_extension" -> ()
      end;
      Spec.parse Spec_parser.spec Spec_lexer.token !Flag.spec_file
    in
    let spec2 =
      if !Flag.comment_spec
      then Spec.parse_comment Spec_parser.spec Spec_lexer.token !Flag.filename
      else Spec.init
    in
    if spec2 <> Spec.init then Flag.use_filter := true;
    Spec.merge spec1 spec2
    |@ not !Flag.only_result &> Spec.print
  in
  let main_fun,arg_num,set_target =
    if !Flag.cegar = Flag.CEGAR_DependentType
    then trans_and_print Trans.set_target "set_target" (fun (_,_,t) -> t) t
    else "",0,t
  in
  (** Unno: I temporally placed the following code here
            so that we can infer refinement types for a safe program
            with extra parameters added *)
  let t0 =
    if !Flag.relative_complete then
      let t = Trans.lift_fst_snd set_target in
      let t = FpatInterface.insert_extra_param t in (* THERE IS A BUG *)
        if true && !Flag.debug_level > 0 then Format.printf "insert_extra_param (%d added)::@. @[%a@.@.%a@.@."
          (List.length !FpatInterface.params) Syntax.pp_print_term t Syntax.pp_print_term' t;
        t
    else
      set_target
  in
  (**)
  let prog, rmap, get_rtyp, info = preprocess t0 spec in
  match !Flag.cegar with
  | Flag.CEGAR_InteractionType ->
      FpatInterface.verify [] prog;
      assert false;
  | Flag.CEGAR_DependentType ->
      try
        match CEGAR.cegar prog info with
        | _, CEGAR.Safe env ->
            Flag.result := "Safe";
            if not !Flag.exp
            then report_safe env rmap get_rtyp orig t0;
            true
        | _, CEGAR.Unsafe ce ->
            Flag.result := "Unsafe";
            if not !Flag.exp
            then report_unsafe main_fun arg_num ce set_target;
            false
      with
      | Fpat.AbsTypeInfer.FailedToRefineTypes when not !Flag.insert_param_funarg ->
          Flag.insert_param_funarg := true;
          run orig parsed
      | Fpat.AbsTypeInfer.FailedToRefineTypes when not !Flag.relative_complete && not !Flag.disable_relatively_complete_verification ->
          if not !Flag.only_result then Format.printf "@.REFINEMENT FAILED!@.";
          if not !Flag.only_result then Format.printf "Restart with relative_complete := true@.@.";
          Flag.relative_complete := true;
          run orig parsed
      | Fpat.AbsTypeInfer.FailedToRefineTypes ->
          raise Fpat.AbsTypeInfer.FailedToRefineTypes
      | Fpat.RefTypeInfer.FailedToRefineExtraParameters ->
          FpatInterface.params := [];
          Fpat.RefTypeInfer.prev_sol := [];
          Fpat.RefTypeInfer.prev_constrs := [];
          incr Fpat.Global.number_of_extra_params;
          run orig parsed
