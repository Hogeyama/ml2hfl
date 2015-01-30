open CEGAR_syntax

exception TypeBottom

val map_body_def : (t -> t) -> fun_def -> fun_def
val map_body_prog : (t -> t) -> prog -> prog
val map_def_prog : (fun_def -> fun_def) -> prog -> prog

val const_of_bool : bool -> const
val subst : var -> t -> t -> t
val subst_var : var -> var -> t -> t
val subst_map : (var * t) list -> t -> t
val subst_typ : var -> t -> typ -> typ
val subst_typ_map : (var * t) list -> typ -> typ
val arg_num : typ -> int
val pop_main : prog -> prog
val get_arg_env : typ -> var list -> (var * typ) list
val put_into_if : prog -> prog
val eta_expand_def : env -> fun_def -> fun_def
val eta_expand : prog -> prog
val get_const_typ : const -> typ
val get_typ : env -> t -> typ
val get_arg_num : typ -> int
val has_bottom : t -> bool
val normalize_bool_term : ?imply:(t list -> t -> bool) -> t -> t
val get_nonrec : fun_def list -> var -> var list -> var list -> (var * t) list
val print_prog_typ' : var list -> var list -> Format.formatter -> prog -> unit
val eval_step_by_step : prog -> 'a
val initialize_env : prog -> prog
val has_no_effect : t -> bool
val assoc_renv : int -> env -> t -> t list
val mem_assoc_renv : int -> env -> bool
val add_renv : (int * (t -> t list)) list -> env -> env
val assign_id_to_rand : prog -> prog
