(** Flags for verifier *)

let debug = ref true

(** {6 Flags for predicate discovery} *)

let generalize_predicates = ref false
let use_min_unsat_prefix = ref true

let refine_only_int = ref true

(** {6 Flags for abstraction type construction} *)
let extract_atomic_predicates = ref false

(** Deprecated flags *)

let refine = (*`IntType *) `RefType
let enable_quick_inference = false

(** constraint solving *)
let use_bit_vector = ref true
