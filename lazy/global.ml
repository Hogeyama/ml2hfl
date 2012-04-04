(** Global variables *)

let debug = ref 2

(** {6 Options for abstraction type inference} *)
let refine = (*`IntType *) `RefType
let extract_atomic_predicates = ref false

(** {6 Options for refinement type inference} *)
let generalize_predicates = ref false
let use_min_unsat_prefix = ref true
let refine_unit = ref true
let refine_function = ref false

(** {6 Options for non-linear constraint solving} *)
let use_bit_vector = ref true

(** {6 Options for deprecated old refinement type inference method} *)
let enable_quick_inference = false
