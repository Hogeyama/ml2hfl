let init () =
  Syntax.typ_excep := Type.TConstr("exn",true)


let preprocess t spec =
  let fun_list,t,get_rtyp =
    if !Flag.init_trans
    then
      let t' = Trans.make_ext_funs t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "make_ext_funs::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let t' = Trans.copy_poly_funs t in
      let fun_list = Syntax.get_top_funs t' in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "copy_poly::@. @[%a@.@." Syntax.pp_print_term' t' in
      let t = t' in
      let spec' = Spec.rename spec t in
      let () = Spec.print spec' in
      let t' = Trans.replace_typ spec'.Spec.abst_env t in
      let () =
        if true && !Flag.debug_level > 0 && spec <> Spec.init
        then Format.printf "add_preds::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let t' = Abstract.abstract_recdata t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "abst_recdata::@. @[%a@.@." Syntax.pp_print_term t' in
      let t = t' in
      let t',get_rtyp_list = Abstract.abstract_list t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "abst_list::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let get_rtyp = get_rtyp_list in
      let t' = Trans.inlined_f spec'.Spec.inlined_f t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "inlined::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let t',get_rtyp_cps_trans = CPS.trans t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "CPS::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let get_rtyp f typ = get_rtyp f (get_rtyp_cps_trans f typ) in
      let t',get_rtyp_remove_pair = CPS.remove_pair t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "remove_pair::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let get_rtyp f typ = get_rtyp f (get_rtyp_remove_pair f typ) in
      let t = t' in
      let t' = if !Flag.insert_param_funarg then Trans.insert_param_funarg t else t in
      let () =
        if !Flag.debug_level > 0 && t <> t'
        then Format.printf "insert unit param::@. @[%a@.@." Syntax.pp_print_term t'
      in
      fun_list, t', get_rtyp
    else Syntax.get_top_funs t, t, fun _ typ -> typ
  in

  let () = Type_check.check t Type.TUnit in
  let prog,map,rmap,get_rtyp_trans = CEGAR_util.trans_prog t in
  let get_rtyp f typ = get_rtyp f (get_rtyp_trans f typ) in

  let info =
    let fun_list =
      let aux x =
        try [List.assoc (CEGAR_util.trans_var x) map] with Not_found -> []
      in
        Util.rev_flatten_map aux fun_list
    in
    let inlined = List.map CEGAR_util.trans_var spec.Spec.inlined in
      {CEGAR.orig_fun_list=fun_list; CEGAR.inlined=inlined}
  in
    prog, rmap, get_rtyp, info



let rec run orig parsed =
  let () = init () in
  let t = parsed in
  let () =
    if false && !Flag.debug_level > 0
    then Format.printf "parsed::@. @[%a@.@." Syntax.pp_print_term' t
  in
  let () =
    if !Flag.use_spec && !Flag.spec_file = ""
    then
      try
        let spec = Filename.chop_extension !Flag.filename ^ ".spec" in
        if Sys.file_exists spec then Flag.spec_file := spec
      with Invalid_argument "Filename.chop_extension" -> ()
  in
  let spec = Spec.parse Spec_parser.spec Spec_lexer.token !Flag.spec_file in
  let () = Spec.print spec in
  let main_fun,arg_num,t = if !Flag.cegar = Flag.CEGAR_DependentType then Trans.set_target t else "",0,t in
  let set_target = t in
  let () =
    if true && !Flag.debug_level > 0
    then Format.printf "set_target::@. @[%a@.@." Syntax.pp_print_term t
  in
  (** Unno: I temporally placed the following code here
            so that we can infer refinement types for a safe program
            with extra parameters added *)
  let t0 =
    if !Flag.relative_complete then
      let t = Trans.lift_fst_snd t in
      let t = FpatInterface.insert_extra_param t in (* THERE IS A BUG *)
        if true && !Flag.debug_level > 0 then Format.printf "insert_extra_param (%d added)::@. @[%a@.@.%a@.@."
          (List.length !FpatInterface.params) Syntax.pp_print_term t Syntax.pp_print_term' t;
        t
    else
      t
  in
  (**)
  let prog, rmap, get_rtyp, info = preprocess t0 spec in
    match !Flag.cegar with
        Flag.CEGAR_InteractionType ->
          FpatInterface.verify [] prog;
          assert false;
      | Flag.CEGAR_DependentType ->
          try
            match CEGAR.cegar prog info with
                prog', CEGAR.Safe env ->
                  if Flag.print_ref_typ
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
                        | _ -> Format.printf "unimplemented or bug@.@."; []
                    in
                    if !Flag.insert_param_funarg
                    then []
                    else
                      if !Flag.relative_complete then
                        let _ = Flag.web := true in
                        let res = Util.rev_map_flatten aux env in
                        let _ = Flag.web := false in
                        res
                      else
                        Util.rev_map_flatten aux env
                  in
                  let () =
                    if !Flag.write_annot
                    then
                      let env'' = List.map (fun (id, typ) -> Id.name id, typ) env' in
                        WriteAnnot.f !Flag.filename orig env''
                  in
                  Format.printf "Safe!@.@.";
                  let () =
                    if !Flag.relative_complete then begin
                      let map =
                        List.map
                          (fun (x, n) ->
                            Id.make (-1) (Fpat.Var.string_of x) Type.TInt,
                            CEGAR_util.trans_inv_term
                              (FpatInterface.inv_term
                                (Fpat.IntTerm.make n)))
                          !Fpat.ParamSubstInfer.ext_coeffs
                      in
                      let t = Syntax.subst_map map t0 in
                      Format.printf "Program with Quantifiers Added:@.";
                      Flag.web := true;
                      Format.printf "  @[<v>%a@]@.@." Syntax.pp_print_term t;
                      Flag.web := false
                    end
                  in
                  if env' <> [] then Format.printf "Refinement Types:@.";
                  let env' = List.map (fun (f, typ) -> f, FpatInterface.simplify typ) env' in
                  let pr (f,typ) =
                    Format.printf "  %s: %a@." (Id.name f) Ref_type.print typ
                  in
                  List.iter pr env';
                  if env' <> [] then Format.printf "@.";
                  true
              | _, CEGAR.Unsafe ce ->
                Format.printf "Unsafe!@.@.";
                if main_fun <> "" && arg_num <> 0
                then
                  Format.printf "Input for %s:@.  %a@." main_fun
                    (Util.print_list Format.pp_print_int "; " false) (Util.take ce arg_num);
                Format.printf "@[<v 2>Error trace:%a@."  Eval.print (ce,set_target);
                false
          with
              Fpat.AbsTypeInfer.FailedToRefineTypes when not !Flag.insert_param_funarg ->
                Flag.insert_param_funarg := true;
                run orig parsed
            | Fpat.AbsTypeInfer.FailedToRefineTypes when not !Flag.relative_complete && not !Flag.disable_relatively_complete_verification ->
                Format.printf "@.REFINEMENT FAILED!@.";
                Format.printf "Restart with relative_complete := true@.@.";
                Flag.relative_complete := true;
                run orig parsed
            | Fpat.AbsTypeInfer.FailedToRefineTypes ->
                raise Fpat.AbsTypeInfer.FailedToRefineTypes
            | Fpat.ParamSubstInfer.FailedToRefineExtraParameters ->
                FpatInterface.params := [];
                Fpat.ParamSubstInfer.ext_coeffs := [];
                Fpat.ParamSubstInfer.ext_constrs := [];
                incr Fpat.Global.number_of_extra_params;
                run orig parsed