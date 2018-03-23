open Util
open Problem

let expand_let_val {term; env; attr; kind} =
  assert (List.mem ACPS attr);
  let term = Trans.expand_let_val term in
  {term; env; attr; kind}

let beta_reduce {term; env; attr; kind} =
  assert (List.mem ACPS attr);
  let term = Trans.expand_let_val term in
  {term; env; attr; kind}

let set_main {term; env; attr; kind} =
  match kind with
  | Safety ->
      let term = Trans.set_main term in
      {term; env; attr; kind}
  | Ref_type_check asrt ->
      let term = Trans.ref_to_assert asrt term in
      {term; env; attr; kind=Safety}

let make_ext_funs {term; env; attr; kind} =
  let term = Trans.make_ext_funs env term in
  {term; env; attr; kind}

let extract_module = map Trans.extract_module
let unify_app = map Trans.unify_app
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
let replace_base_with_int = map Trans.replace_base_with_int
let inline_simple_types = map Trans.inline_simple_types
let replace_data_with_int = map Trans.replace_data_with_int
let inline_simple_types = map Trans.inline_simple_types
let inline_type_decl = map Trans.inline_type_decl
let replace_bottom_def = map Trans.replace_bottom_def
let insert_param_funarg = map Trans.insert_param_funarg
let alpha_rename ~set_counter = map @@ Trans.alpha_rename ~whole:true ~set_counter
let copy_poly_funs = map_on Focus.fst Trans.copy_poly_funs
let insert_param_funarg = map Trans.insert_param_funarg
let ignore_exn_arg = map Trans.ignore_exn_arg
let ref_to_assert env = map (Trans.ref_to_assert env)
