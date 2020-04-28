open Util
open Problem

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

let extract_module problem =(*
  let tr_ty =
    let env = col_type_decls @@ Problem.term problem in
    Term_util.subst_tdata_typ_map
  in
  let tr_env env = List.map (Pair.map_snd tr_ty) env in*)
  map Trans.extract_module problem
let inline_module_var = map Trans.inline_module_var
let instansiate_poly_types = map Trans.instansiate_poly_types
let mark_fv_as_external = map Trans.mark_fv_as_external
let alpha_rename ~whole ~set_counter = map @@ Trans.alpha_rename ~whole ~set_counter
let elim_unused_let ~leave_last = map @@ Trans.elim_unused_let ~leave_last
let lift_type_decl = map Trans.lift_type_decl
let inline_simple_types = map Trans.inline_simple_types
let inline_record_type = map Trans.inline_record_type
let ignore_non_termination = map Trans.ignore_non_termination
let beta_reduce_trivial = map Trans.beta_reduce_trivial
let elim_redundant_arg = map Trans.elim_redundant_arg
let recover_const_attr = map Trans.recover_const_attr
let decomp_pair_eq = map Trans.decomp_pair_eq
let ignore_exn_arg = map Trans.ignore_exn_arg
let replace_base_with_int =
  let tr_env env = List.map (Pair.map_snd Ref_type.replace_base_with_int) env in
  map ~tr_env Trans.replace_base_with_int
let inline_simple_types = map Trans.inline_simple_types
let replace_data_with_int =
  let tr_env env = List.map (Pair.map_snd Ref_type.replace_data_with_int) env in
  map ~tr_env Trans.replace_data_with_int
let replace_complex_data_with_int = map Trans.replace_complex_data_with_int
let inline_simple_types = map Trans.inline_simple_types
let inline_type_decl = map Trans.inline_type_decl
let replace_bottom_def = map Trans.replace_bottom_def
let insert_param_funarg = map Trans.insert_param_funarg
let copy_poly_funs = map_on Focus.fst Trans.copy_poly_funs
let insert_param_funarg = map Trans.insert_param_funarg
let ignore_exn_arg = map Trans.ignore_exn_arg
let ref_to_assert env = map (Trans.ref_to_assert env)
let split_type_decls = map Trans.split_type_decls
let abst_literal = map Trans.abst_literal
let encode_bool_as_int = map Trans.encode_bool_as_int
let reduce_rand = map Trans.reduce_rand
let reduce_ignore = map Trans.reduce_ignore
let reduce_branch = map Trans.reduce_branch
let variant_args_to_tuple = map Trans.variant_args_to_tuple
let unify_pure_fun_app = map Trans.unify_pure_fun_app
let add_occurence_param = map Trans.add_occurence_param
let slice = map Slice.slice
let remove_not = map Trans.remove_not
let inline = map (Trans.inlined_f())
let beta_reduce' = map Trans.beta_reduce

let split_assert {term; env; attr; kind; info} =
  let update_info loc info =
    match loc with
    | None -> info
    | Some loc -> Format.asprintf "Target assertion: %a" Location.print_loc loc :: info
  in
  Trans.split_assert term
  |> List.map (fun (term,loc) -> {term; env; attr; kind; info=update_info loc info})

let expand_let_val {term; env; attr; kind; info} =
  assert (List.mem ACPS attr);
  let term = Trans.expand_let_val term in
  {term; env; attr; kind; info}

let beta_reduce {term; env; attr; kind; info} =
  assert (List.mem ACPS attr);
  let term = Trans.expand_let_val term in
  {term; env; attr; kind; info}

let set_main {term; env; attr; kind; info} =
  match kind with
  | Safety ->
      let term = Trans.set_main term in
      [{term; env; attr; kind; info}]
  | Ref_type_check check ->
      let open Term_util in
      let make_check (x, ty) =
        let tys,ty' = Ref_type.decomp_funs ty in
        let r = Id.new_var ~name:"r" @@ Ref_type.to_simple ty' in
        let t_check =
          match ty' with
          | Ref_type.Base(base,y,p) -> subst_var y r p
          | _ -> Term.true_
        in
        let xs =
          let aux = Id.new_var ~name:"arg" -| Ref_type.to_simple ~with_pred:true in
          List.map (snd |- aux) tys
        in
        let t_check' = List.fold_right2 (subst_var -| fst) tys xs t_check in
        let main = Term.(let_ [r, var x @ vars xs] (assert_ t_check')) in
        let env =
          let aux x (y,ty) acc = (x,ty) :: List.map (Pair.map_snd @@ Ref_type.subst_var y x) acc in
          List.fold_right2 aux xs tys []
        in
        env, Trans.replace_main ~main term
      in
      check
      |> List.map make_check
      |> List.map (fun (env',term) -> {term; env=env'@env; attr; kind=Safety; info})

let make_ext_funs {term; env; attr; kind; info} =
  let term = Trans.make_ext_funs env term in
  {term; env=[]; attr; kind; info}

let alpha_rename {term; env; attr; kind; info} =
  let map_rtyp map get_rtyp f = get_rtyp f in
  let term = Trans.alpha_rename ~whole:true ~set_counter:true term in
  let map = [] in (* TODO *)
  {term; env; attr; kind; info}, map_rtyp map

let split_by_ref_type spec {term; env; attr; kind; info} =
  match kind with
  | Safety ->
      let aux (target, term) =
        let env = spec @ env in
        match target with
        | None -> [{term; env; attr; kind=Safety; info}]
        | Some (f,ty) ->
            let info = Format.asprintf "Check %a: %a" Print.id f Ref_type.print ty :: info in
            let env = List.filter_out (fun (g,_) -> Id.(f = g)) env in
            set_main {term; env; attr; kind=Ref_type_check [f,ty]; info}
      in
      term
      |> Trans.split_by_ref_type spec
      |> List.flatten_map aux
  | Ref_type_check check -> assert false

let slice_top_fun ?(num = !Flag.Method.slice_num) problem =
  let make p =
    let term = Slice.slice_top_fun_with_context problem.Problem.term p in
    {problem with term}, Fun.id
  in
  List.init num (fun i -> float (i+1) /. float num)
  |> List.map make
