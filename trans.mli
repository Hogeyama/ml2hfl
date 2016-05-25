open Syntax

val flatten_tvar : typed_term -> typed_term
val inst_tvar_tunit_typ : typ -> typ
val inst_tvar_tunit : typed_term -> typed_term
val get_tvars : typ -> typ option ref list
val rename_poly_funs : id -> typed_term -> (id * id) list * typed_term
val copy_poly_funs : typed_term -> typed_term * ((Syntax.id -> Ref_type.t) -> Syntax.id -> Ref_type.t)
val define_randvalue : ((typ * id) list * (id * id list * typed_term) list) -> typ -> ((typ * id) list * (id * id list * typed_term) list) * typed_term
val inst_randval : typed_term -> typed_term
val ref_to_assert : Ref_type.env -> typed_term -> typed_term
val replace_main : ?force:bool -> typed_term -> typed_term -> typed_term
val map_main : (typed_term -> typed_term) -> typed_term -> typed_term
val set_main : typed_term -> (string * int) option * typed_term
val merge_let_fun : typed_term -> typed_term
val canonize : typed_term -> typed_term
val part_eval : typed_term -> typed_term
val trans_let : typed_term -> typed_term
val propagate_typ_arg : typed_term -> typed_term
val replace_typ : (Syntax.id * Syntax.typ) list -> typed_term -> typed_term
val eval : typed_term -> typed_term
val get_and_list : typed_term -> typed_term list
val elim_fun : typed_term -> typed_term
val make_ext_env : typed_term -> env
val init_rand_int : typed_term -> typed_term
val inlined_f : id list -> typed_term -> typed_term
val lift_fst_snd : typed_term -> typed_term
val expand_let_val : typed_term -> typed_term
val insert_param_funarg : typed_term -> typed_term
val search_fail : typed_term -> int list list
val screen_fail : int list -> typed_term -> typed_term
val rename_ext_funs : id list -> typed_term -> id list * typed_term
val make_ext_funs : ?asm:bool -> ?fvs:(id list) -> Ref_type.env -> typed_term -> typed_term
val assoc_typ : id -> typed_term -> typ
val diff_terms : typed_term -> typed_term -> (typed_term * typed_term) list
val remove_label : ?label:string -> typed_term -> typed_term
val alpha_rename : typed_term -> typed_term
val replace_base_with_int : typed_term -> typed_term
val abst_ref : typed_term -> typed_term
val remove_top_por : typed_term -> typed_term
val replace_bottom_def : typed_term -> typed_term
val merge_bound_var_typ : (id * typ) list -> typed_term -> typed_term
val encode_mutable_record : typed_term -> typed_term
val recover_const_attr : typed_term -> typed_term
val recover_const_attr_shallowly : typed_term -> typed_term
val subst_with_rename : ?check:bool -> id -> typed_term -> typed_term -> typed_term
val ignore_non_termination : typed_term -> typed_term
val decomp_var_match_tuple : typed_term -> typed_term
val map_attr : (attr list -> attr list) -> typed_term -> typed_term
val filter_attr : (attr -> bool) -> typed_term -> typed_term
val split_assert : typed_term -> typed_term
val add_id : typed_term -> int * typed_term
val remove_id : typed_term -> typed_term
val replace_fail_with_raise : typed_term -> typed_term
val remove_defs : id list -> typed_term -> typed_term
val subst_let_xy : typed_term -> typed_term


(** {6 Normalization} *)
val normalize_binop_exp : binop -> typed_term -> typed_term -> term
val normalize_bool_exp : typed_term -> typed_term
val normalize_let : ?is_atom:(typed_term -> bool) -> typed_term -> typed_term
val merge_geq_leq : typed_term -> typed_term
val let2fun : typed_term -> typed_term
val fun2let : typed_term -> typed_term
val null_tuple_to_unit : typed_term -> typed_term
val reconstruct : typed_term -> typed_term
val short_circuit_eval : typed_term -> typed_term
val flatten_let : typed_term -> typed_term
val tfuns_to_tfun : typed_term -> typed_term
val tfun_to_tfuns : typed_term -> typed_term
val flatten_tuple : typed_term -> typed_term
val decomp_pair_eq : typed_term -> typed_term
val eta_normal : typed_term -> typed_term
val direct_from_CPS : typed_term -> typed_term

(** {6 Simplification, Inlining, Reduction} *)
val simplify_match : typed_term -> typed_term
val simplify_if_cond : typed_term -> typed_term
val elim_unused_let : ?cbv:bool -> typed_term -> typed_term
val elim_unused_branch : typed_term -> typed_term
val inline_no_effect : typed_term -> typed_term
val inline_var : typed_term -> typed_term
val inline_var_const : typed_term -> typed_term
val inline_simple_exp : typed_term -> typed_term
val inline_next_redex : typed_term -> typed_term
val inline_specified : (id * id list * typed_term) -> typed_term -> typed_term
val beta_no_effect_tuple : typed_term -> typed_term
val beta_var_tuple : typed_term -> typed_term
val beta_reduce_trivial : typed_term -> typed_term
val beta_affine_fun : typed_term -> typed_term
val beta_size1 : typed_term -> typed_term
val beta_reduce : typed_term -> typed_term
val beta_no_effect : typed_term -> typed_term
val reduce_bottom : typed_term -> typed_term
