open Util

module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)


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

type t = preprocess_label * ((results -> bool) * (results -> tr_result))

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

let before label pps =
  List.takewhile ((<>) label -| fst) pps

let and_after label pps =
  List.dropwhile ((<>) label -| fst) pps

let filter_out labels pps =
  List.filter_out (fst |- List.mem -$- labels) pps

let all spec : t list =
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
    Copy_poly,
      (Fun.const true,
       Trans.copy_poly_funs -| last_t);
    Abst_ref,
      (Fun.const true,
       map_trans Encode.abst_ref);
    Make_fun_tuple,
      (Fun.const !Flag.tupling,
       map_trans Ref_trans.make_fun_tuple);
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


let rec trans_and_print f desc proj_in proj_out ?(opt=true) ?(pr=Print.term_typ) t =
  Debug.printf "START: %s@." desc;
  let r = f t in
  Debug.printf "END: %s@." desc;
  let t' = proj_out r in
  if proj_in t <> t' && opt
  then Verbose.printf "###%a:@. @[%a@.@." Color.s_red desc pr t';
  r

let run pps t =
  let aux acc (label,(cond,f)) =
    if cond acc then
      let r = trans_and_print f (string_of_label label) last_t fst acc in
      (label, r)::acc
    else
      acc
  in
  List.fold_left aux [Init, (t, get_rtyp_id)] pps
