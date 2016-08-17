open Util

type result = Safe of (Syntax.id * Ref_type.t) list | Unsafe of int list

module Debug = Debug.Make(struct let cond = Debug.Module "Main_loop" end)

let rec trans_and_print f desc proj_in proj_out ?(opt=true) ?(pr=Print.term_typ) t =
  Debug.printf "START: %s@." desc;
  let r = f t in
  Debug.printf "END: %s@." desc;
  let t' = proj_out r in
  if proj_in t <> t' && opt
  then NORDebug.printf "###%a:@. @[%a@.@." Color.s_red desc pr t';
  r


type preprocess_label =
  | Init
  | Replace_const
  | Encode_mutable_record
  | Encode_record
  | Encode_array
  | Abst_ref
  | Make_fun_tuple
  | Make_ext_funs
  | Copy_poly
  | Ignore_non_termination
  | Beta_reduce_trivial
  | Recover_const_attr
  | Decomp_pair_eq
  | Add_preds
  | Replace_fail_with_raise
  | Encode_variant
  | Encode_recdata
  | Replace_base_with_int
  | Encode_list
  | Ret_fun
  | Ref_trans
  | Tupling
  | Inline
  | CPS
  | Remove_pair
  | Replace_bottom_def
  | Add_cps_preds
  | Eliminate_same_arguments
  | Insert_unit_param
  | Preprocessfortermination

type tr_result = Syntax.term * ((Syntax.id -> Ref_type.t) -> Syntax.id -> Ref_type.t)

type results = (preprocess_label * tr_result) list

type preprocess = preprocess_label * ((results -> bool) * (results -> tr_result))

let string_of_label = function
  | Init -> "Init"
  | Replace_const -> "Replace_const"
  | Encode_mutable_record -> "Encode_mutable_record"
  | Encode_record -> "Encode_record"
  | Encode_array -> "Encode_array"
  | Abst_ref -> "Abst_ref"
  | Make_fun_tuple -> "Make_fun_tuple"
  | Make_ext_funs -> "Make_ext_funs"
  | Copy_poly -> "Copy_poly"
  | Ignore_non_termination -> "Ignore_non_termination"
  | Beta_reduce_trivial -> "Beta_reduce_trivial"
  | Recover_const_attr -> "Recover_const_attr"
  | Decomp_pair_eq -> "Decomp_pair_eq"
  | Add_preds -> "Add_preds"
  | Replace_fail_with_raise -> "Replace_fali_with_raise"
  | Encode_variant -> "Encode_variant"
  | Encode_recdata -> "Encode_recdata"
  | Replace_base_with_int -> "Replace_base_with_int"
  | Encode_list -> "Encode_list"
  | Ret_fun -> "Ret_fun"
  | Ref_trans -> "Ref_trans"
  | Tupling -> "Tupling"
  | Inline -> "Inline"
  | CPS -> "CPS"
  | Remove_pair -> "Remove_pair"
  | Replace_bottom_def -> "Replace_bottom_def"
  | Add_cps_preds -> "Add_cps_preds"
  | Eliminate_same_arguments -> "Eliminate_same_arguments"
  | Insert_unit_param -> "Insert_unit_param"
  | Preprocessfortermination -> "Preprocessfortermination"

let last acc = snd @@ List.hd acc
let last_t acc = fst @@ last acc
let last_get_rtyp acc = snd @@ last acc
let take_result l acc = fst @@ List.assoc l acc

let get_rtyp_id get_rtyp f = get_rtyp f

let map_trans tr acc = tr @@ last_t acc, get_rtyp_id
let preprocesses spec : preprocess list =
  [
    Replace_const,
      (Fun.const !Flag.replace_const,
       map_trans CFA.replace_const);
    Encode_mutable_record,
      (Fun.const true,
       map_trans Encode.mutable_record);
    Encode_record,
      (Fun.const true,
       map_trans Encode.record);
    Encode_array,
      (Fun.const true,
       map_trans Encode.array);
    Abst_ref,
      (Fun.const true,
       map_trans Encode.abst_ref);
    Make_fun_tuple,
      (Fun.const !Flag.tupling,
       map_trans Ref_trans.make_fun_tuple);
    Copy_poly,
      (Fun.const true,
       Trans.copy_poly_funs -| last_t);
    Make_ext_funs,
      (Fun.const true,
       fun acc -> Trans.make_ext_funs (Spec.get_ext_ref_env spec @@ last_t acc) @@ last_t acc, get_rtyp_id);
    Ignore_non_termination,
      (Fun.const !Flag.ignore_non_termination,
       map_trans Trans.ignore_non_termination);
    Beta_reduce_trivial,
      (Fun.const true,
       map_trans Trans.beta_reduce_trivial);
    Recover_const_attr,
      (Fun.const true,
       map_trans Trans.recover_const_attr);
    Decomp_pair_eq,
      (Fun.const true,
       map_trans Trans.decomp_pair_eq);
    Add_preds,
      (Fun.const (spec.Spec.abst_env <> []),
       fun acc -> Trans.replace_typ (Spec.get_abst_env spec @@ last_t acc) @@ last_t acc, get_rtyp_id);
    Replace_fail_with_raise,
      (Fun.const !Flag.fail_as_exception,
       map_trans Trans.replace_fail_with_raise);
    Encode_variant,
      (Fun.const true,
       map_trans Encode.variant);
    Encode_recdata,
      (Fun.const true,
       map_trans Encode.recdata);
    Replace_base_with_int,
      (Fun.const !Flag.base_to_int,
       map_trans Trans.replace_base_with_int);
    Encode_list,
      (Fun.const true,
       Encode.list -| last_t);
    Ret_fun,
      (Fun.const !Flag.tupling,
       Ret_fun.trans -| last_t);
    Ref_trans,
      (Fun.const !Flag.tupling,
       Ref_trans.trans -| last_t);
    Tupling,
      (Fun.const !Flag.tupling,
       Tupling.trans -| last_t);
    Inline,
      (Fun.const true,
       (fun acc -> let t = last_t acc in Trans.inlined_f (Spec.get_inlined_f spec t) t, get_rtyp_id));
    CPS,
      (Fun.const !Flag.trans_to_CPS,
       CPS.trans -| last_t);
    Remove_pair,
      (Fun.const !Flag.trans_to_CPS,
       Curry.remove_pair -| last_t);
    Replace_bottom_def,
      (Fun.const true,
       map_trans Trans.replace_bottom_def);
    Add_preds,
      (Fun.const (spec.Spec.abst_cps_env <> []),
       fun acc -> Trans.replace_typ (Spec.get_abst_cps_env spec @@ last_t acc) @@ last_t acc, get_rtyp_id);
    Eliminate_same_arguments,
      (Fun.const !Flag.elim_same_arg,
       map_trans Elim_same_arg.trans);
    Insert_unit_param,
      (Fun.const !Flag.insert_param_funarg,
       map_trans Trans.insert_param_funarg);
    Preprocessfortermination,
      (Fun.const (!Flag.mode = Flag.Termination),
       map_trans !BRA_types.preprocessForTerminationVerification);
  ]


let preprocess_before label pps =
  List.takewhile ((<>) label -| fst) pps

let preprocess_and_after label pps =
  List.dropwhile ((<>) label -| fst) pps

let preprocess_filter_out labels pps =
  List.filter_out (fst |- List.mem -$- labels) pps

let run_preprocess pps t =
  let aux acc (label,(cond,f)) =
    if cond acc then
      let r = trans_and_print f (string_of_label label) last_t fst acc in
      (label, r)::acc
    else
      acc
  in
  List.fold_left aux [Init, (t, get_rtyp_id)] pps

let preprocess ?(make_pps=None) ?(fun_list=None) t spec =
  let pps' =
    match make_pps with
    | None -> preprocesses spec
    | Some make_pps' -> make_pps' spec
  in
  let results = run_preprocess pps' t in
  let t = last_t results in
  let fun_list' =
    match fun_list with
    | None -> Term_util.get_top_funs @@ take_result Decomp_pair_eq results
    | Some fun_list' -> fun_list'
  in

  if !Flag.exp2 then
    begin
      let oc = open_out @@ Filename.change_extension !Flag.filename "pml" in
      let ocf = Format.formatter_of_out_channel oc in
      Format.fprintf ocf "%a@." Print.term_typ t;
      close_out oc
    end;

  let prog,map,_,make_get_rtyp_trans = CEGAR_trans.trans_prog (*~spec:abst_cegar_env*) t in
  let abst_cegar_env =
    Spec.get_abst_cegar_env spec prog
    |@(not !Flag.only_result)&> Spec.print_abst_cegar_env Format.std_formatter
  in
  let prog = CEGAR_trans.add_env abst_cegar_env prog in
  let make_get_rtyp =
    if !!Debug.check then
      let aux f (label,(_,g)) map x =
        Format.printf "BEGIN %s@." @@ string_of_label label;
        let r = try g (f map) x with _ -> Format.printf "GET_RTYP ERROR: %s@." @@ string_of_label label; assert false in
        Format.printf "END %s@." @@ string_of_label label;
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
      if !Flag.mode = Flag.FairNonTermination then
        Some spec.Spec.fairness
      else
        None
    in
    {prog.CEGAR_syntax.info with CEGAR_syntax.orig_fun_list; CEGAR_syntax.inlined; CEGAR_syntax.fairness}
  in
  {prog with CEGAR_syntax.info}, make_get_rtyp



let write_annot env orig =
  env
  |> List.map (Pair.map_fst Id.name)
  |> WriteAnnot.f !Flag.filename orig

let report_safe env orig t0 =
  if !Flag.write_annot then write_annot env orig;

  let s =
    match !Flag.mode with
    | Flag.NonTermination -> "Non-terminating!"
    | Flag.FairNonTermination -> "Fair Infinite Execution found!"
    | _ -> "Safe!"
  in
  Color.printf Color.Bright "%s@.@." s;

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
      Ref.tmp_set Flag.web true (fun () -> Format.printf "  @[<v>%a@]@.@." Print.term t)
    end;

  let only_result_termination = not !!NORDebug.check && !Flag.mode = Flag.Termination in
  if env <> [] && not only_result_termination then
    begin
      Format.printf "Refinement Types:@.";
      let env' = List.map (Pair.map_snd Ref_type.simplify) env in
      let pr (f,typ) = Format.printf "  %s: %a@." (Id.name f) Ref_type.print typ in
      List.iter pr env';
      Format.printf "@.";

      if !Flag.print_abst_typ then
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


let rec run_cegar prog =
  try
    match CEGAR.run prog with
    | CEGAR.Safe env ->
        Flag.result := "Safe";
        Color.printf Color.Bright "Safe!@.@.";
        true
    | CEGAR.Unsafe _ ->
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


let insert_extra_param t =
  let t' =
    t
    |> Trans.lift_fst_snd
    |> FpatInterface.insert_extra_param (* THERE IS A BUG in exception handling *)
  in
  if true then
    NORDebug.printf "insert_extra_param (%d added)::@. @[%a@.@.%a@.@."
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

let rec loop ?(make_pps=None) ?(fun_list=None) exparam_sol ?(spec=Spec.init) parsed set_target =
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
  let prog, make_get_rtyp = preprocess ~make_pps ~fun_list set_target' spec in
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
    let result = CEGAR.run prog' in
    result, make_get_rtyp, set_target'
  with e ->
    improve_precision e;
    loop ~make_pps ~fun_list exparam_sol ~spec parsed set_target


let trans_env top_funs make_get_rtyp env : (Syntax.id * Ref_type.t) list =
  let get_rtyp f = List.assoc f env in
  let aux f = Option.try_any (fun _ -> f, Ref_type.rename @@ make_get_rtyp get_rtyp f) in
  List.filter_map aux top_funs

let verify ?(make_pps=None) ?(fun_list=None) exparam_sol spec parsed =
  let main,set_target =
    if spec.Spec.ref_env = [] then
      trans_and_print Trans.set_main "set_main" Fun.id snd parsed
    else
      let ref_env =
        Spec.get_ref_env spec parsed
        |@ not !Flag.only_result &> Spec.print_ref_env Format.std_formatter
        |> Ref_type.Env.of_list
      in
      None, trans_and_print (Trans.ref_to_assert ref_env) "ref_to_assert" Fun.id Fun.id parsed
  in
  loop ~make_pps ~fun_list exparam_sol ~spec parsed set_target, main, set_target


let run ?(make_pps=None) ?(fun_list=None) orig exparam_sol ?(spec=Spec.init) parsed =
  let (result, make_get_rtyp, set_target'), main, set_target = verify ~make_pps ~fun_list exparam_sol spec parsed in
  match result with
  | CEGAR.Safe env ->
      Flag.result := "Safe";
      let env' = trans_env (Term_util.get_top_funs parsed) make_get_rtyp env in
      if not !Flag.exp && !Flag.mode = Flag.FairTermination => !!NORDebug.check then
        report_safe env' orig set_target';
      true
  | CEGAR.Unsafe(sol,_) ->
      Flag.result := "Unsafe";
      if not !Flag.exp then
        report_unsafe main sol set_target;
      false
