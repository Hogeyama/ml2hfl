(** Global variables *)

let debug = ref true
let debug_level = ref 10

let current_log_level = ref 0
let log_begin str =
  let _ = current_log_level := !current_log_level + 1 in
  if !debug && !debug_level >= !current_log_level then Format.printf "begin %s[%d]@,  @[<v>" str !current_log_level
let log f =
  if !debug && !debug_level >= !current_log_level then f ()
let log_end str =
  let _ = if !debug && !debug_level >= !current_log_level then Format.printf "@]@,end %s[%d]@," str !current_log_level in
  current_log_level := !current_log_level - 1

(** {6 Options for abstraction type inference} *)
let refine = (*`IntType *) `RefType
let extract_atomic_predicates = ref false

(** {6 Options for refinement type inference} *)
let generalize_predicates = ref false
let generalize_predicates_simple = ref false
let find_preds_forward = ref false
let subst_hcs_inc = ref false

let generalize_predicates = ref false
let use_min_unsat_prefix = ref true
let refine_unit = ref true
let refine_function = ref false
let no_inlining = ref false
let inline_after_ncs = ref false
let fol_backward = ref false

let disable_pred_sharing1 = ref false
let enable_pred_sharing2 = ref false

let coeff_const = ref false

let flag_coeff = ref false

let number_of_extra_params = ref 1
let bits_threshold = ref 2

(** {6 Options for non-linear constraint solving} *)
let use_bit_vector = ref true

(** {6 Options for deprecated old refinement type inference method} *)
let enable_quick_inference = false
