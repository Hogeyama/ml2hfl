open Syntax

val flatten_tvar : term -> term
val inst_tvar_tunit_typ : typ -> typ
val inst_tvar_tunit : term -> term
val inst_tvar_tint_typ : typ -> typ
val inst_tvar_tint : term -> term
val get_tvars : typ -> typ option ref list
val rename_poly_funs : id -> term -> (id * id) list * term
val copy_poly_funs : term -> term * ((Syntax.id -> Ref_type.t) -> Syntax.id -> Ref_type.t)
val define_randvalue : ((typ * id) list * (id * id list * term) list) -> typ -> ((typ * id) list * (id * id list * term) list) * term
val inst_randval : term -> term
val ref_to_assert : ?make_fail:(typ -> term) -> ?typ_exn:typ -> Ref_type.env -> term -> term
val replace_main : ?force:bool -> term -> term -> term
val map_main : (term -> term) -> term -> term
val set_main : term -> (string * int) option * term
val merge_let_fun : term -> term
val canonize : term -> term
val part_eval : term -> term
val trans_let : term -> term
val propagate_typ_arg : term -> term
val replace_typ : (Syntax.id * Syntax.typ) list -> term -> term
val eval : term -> term
val get_and_list : term -> term list
val elim_fun : term -> term
val make_ext_env : term -> env
val init_base_rand : term -> term (** replace rand_int() with a fresh variable*)
val inlined_f : id list -> term -> term
val lift_fst_snd : term -> term
val expand_let_val : term -> term
val insert_param_funarg : term -> term
val search_fail : term -> int list list
val screen_fail : int list -> term -> term
val rename_ext_funs : id list -> term -> id list * term
val make_ext_funs : ?fvs:(id list) -> Ref_type.env -> term -> term
val assoc_typ : id -> term -> typ
val diff_terms : term -> term -> (term * term) list
val remove_label : ?label:string -> term -> term
val alpha_rename : term -> term
val replace_base_with_int : term -> term
val remove_top_por : term -> term
val replace_bottom_def : term -> term
val merge_bound_var_typ : (id * typ) list -> term -> term
val recover_const_attr : term -> term
val recover_const_attr_shallowly : term -> term
val subst_with_rename : ?check:bool -> id -> term -> term -> term
val ignore_non_termination : term -> term
val decomp_var_match_tuple : term -> term
val map_attr : (attr list -> attr list) -> term -> term
val filter_attr : (attr -> bool) -> term -> term
val split_assert : term -> term
val add_id : term -> int * term
val add_id_if : (term -> bool) -> term -> int * term
val remove_id : term -> term
val replace_fail_with : desc -> term -> term
val replace_fail_with_raise : term -> term
val remove_defs : id list -> term -> term
val subst_let_xy : term -> term


(** {6 Normalization} *)
val normalize_binop_exp : binop -> term -> term -> desc
val normalize_bool_exp : term -> term
val normalize_let : ?is_atom:(term -> bool) -> term -> term
val merge_geq_leq : term -> term
val let2fun : term -> term
val fun2let : term -> term
val null_tuple_to_unit : term -> term
val reconstruct : term -> term
val short_circuit_eval : term -> term
val flatten_let : term -> term
val tfuns_to_tfun : term -> term
val tfun_to_tfuns : term -> term
val flatten_tuple : term -> term
val decomp_pair_eq : term -> term
val eta_normal : term -> term
val direct_from_CPS : term -> term

(** {6 Simplification, Inlining, Reduction} *)
val simplify_match : term -> term
val simplify_if_cond : term -> term
val elim_unused_let : ?leave_last:bool -> ?cbv:bool -> term -> term
val elim_unused_branch : term -> term
val inline_no_effect : term -> term
val inline_var : term -> term
val inline_var_const : term -> term
val inline_simple_exp : term -> term
val inline_next_redex : term -> term
val inline_specified : (id * id list * term) -> term -> term
val beta_no_effect_tuple : term -> term
val beta_var_tuple : term -> term
val beta_reduce_trivial : term -> term
val beta_affine_fun : term -> term
val beta_size1 : term -> term
val beta_reduce : term -> term
val beta_no_effect : term -> term
val reduce_bottom : term -> term
val reduce_fail_unit : term -> term
val remove_no_effect_trywith : term -> term
val bool_eta_reduce : term -> term
val eta_tuple : term -> term
val eta_reduce : term -> term
