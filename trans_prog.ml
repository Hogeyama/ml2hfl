open Util
open Program

let expand_let_val {term; env; attr} =
  assert (List.mem ACPS attr);
  let term = Trans.expand_let_val term in
  {term; env; attr}

let beta_reduce {term; env; attr} =
  assert (List.mem ACPS attr);
  let term = Trans.expand_let_val term in
  {term; env; attr}

let set_main {term; env; attr} =
  let term = Fun.cond (env <> Ref_type.Env.empty) (Trans.ref_to_assert env) term in
  {term; env; attr}

let extract_module = Program.map Trans.extract_module
let unify_app = Program.map Trans.unify_app
let mark_fv_as_external = Program.map Trans.mark_fv_as_external
let alpha_rename ~whole ~set_counter = Program.map @@ Trans.alpha_rename ~whole ~set_counter
let elim_unused_let ~leave_last = Program.map @@ Trans.elim_unused_let ~leave_last
let lift_type_decl = Program.map Trans.lift_type_decl
let inline_simple_types = Program.map Trans.inline_simple_types
let inline_record_type = Program.map Trans.inline_record_type
let ignore_non_termination = Program.map Trans.ignore_non_termination
let beta_reduce_trivial = Program.map Trans.beta_reduce_trivial
let elim_redundant_arg = Program.map Trans.elim_redundant_arg
let recover_const_attr = Program.map Trans.recover_const_attr
let decomp_pair_eq = Program.map Trans.decomp_pair_eq
let ignore_exn_arg = Program.map Trans.ignore_exn_arg
let replace_base_with_int = Program.map Trans.replace_base_with_int
let inline_simple_types = Program.map Trans.inline_simple_types
let replace_data_with_int = Program.map Trans.replace_data_with_int
let inline_simple_types = Program.map Trans.inline_simple_types
let inline_type_decl = Program.map Trans.inline_type_decl
let replace_bottom_def = Program.map Trans.replace_bottom_def
let insert_param_funarg = Program.map Trans.insert_param_funarg
let alpha_rename ~set_counter = Program.map @@ Trans.alpha_rename ~whole:true ~set_counter
let copy_poly_funs = Program.map_on Focus.fst Trans.copy_poly_funs
let insert_param_funarg = Program.map Trans.insert_param_funarg
let ignore_exn_arg = Program.map Trans.ignore_exn_arg
let set_main = Program.map Trans.set_main
let ref_to_assert env = Program.map (Trans.ref_to_assert env)
