(** Flags for verifier *)

let debug = ref true

(** {6 Flags for predicate discovery} *)

let generalize_predicates = ref false

(** {6 Flags fo abstraction type construction} *)
let extract_atomic_predicates = ref false

(** Deprecated flags *)

let refine = (*`IntType *) `RefType
let enable_quick_inference = false
