(** HCCS solver combinators *)

type t = HCCS.t -> PredSubst.t

exception NoSolution
exception UnsolvableCore of string list
exception Unknown

(** {6 Dynamically linked solvers} *)

val ext_init_rsrefine : (PredVar.t list -> unit) ref
val init_rsrefine : PredVar.t list -> unit
val ext_solve_rsrefine_rs1 : t ref
val solve_rsrefine_rs1 : t
val ext_solve_rsrefine_rs4 : t ref
val solve_rsrefine_rs4 : t
val ext_solve_rsrefine_rs8 : t ref
val solve_rsrefine_rs8 : t
val ext_solve_rsrefine_itm : t ref
val solve_rsrefine_itm : t
val ext_solve_rsrefine_exl : t ref
val solve_rsrefine_exl : t
val ext_solve_rsrefine_rse : t ref
val solve_rsrefine_rse : t

val ext_solve_duality : t ref
val solve_duality : t
val ext_solve_pdr : t ref
val solve_pdr : t

val ext_solve_unit : (t -> t) ref
val ext_solve_bool : (t -> t) ref
val ext_solve_tuple : (t -> t) ref

val check_solvability_first : bool ref
val link_dyn : t -> unit
val get_dyn : unit -> t
val solve_dyn : t

val ext_of_string : (string -> t) ref
val of_string_dyn : string -> t
