(** Flags for verifier *)

let debug = ref false

(** {6 Flags for predicate discovery} *)

let generalize_predicates = ref false
let use_min_unsat_prefix = ref true

(** {6 Flags for abstraction type construction} *)
let extract_atomic_predicates = ref false

(** Deprecated flags *)

let refine = (*`IntType *) `RefType
let enable_quick_inference = false
