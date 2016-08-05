open Syntax

val dummy_var : id
val abst_var : id
val abst_var_int : id
val abst_var_bool : id
val length_var : id

val typ_result : typ
val typ_event : typ
val typ_event' : typ
val typ_event_cps : typ
val typ_exn : typ

val make_attr : ?attrs:attr list -> typed_term list -> attr list

(** {6 Term constructor} *)
val unit_term : typed_term
val true_term : typed_term
val false_term : typed_term
val cps_result : typed_term
val fail_term : typed_term
val fail_term_cps : typed_term
val randint_term : typed_term
val randbool_unit_term : typed_term
val randint_unit_term : typed_term
val fail_unit_term : typed_term
val make_bool : bool -> typed_term
val make_bottom : typ -> typed_term
val make_event : string -> typed_term
val make_event_unit : string -> typed_term
val make_event_cps : string -> typed_term
val make_var : id -> typed_term
val make_int : int -> typed_term
val make_string : string -> typed_term
val make_randvalue : typ -> typed_term
val make_randvalue_unit : typ -> typed_term
val make_randvalue_cps : typ -> typed_term
val make_randint_cps : bool -> typed_term
val make_app : typed_term -> typed_term list -> typed_term
val make_app_raw : typed_term -> typed_term list -> typed_term (** Does not merge arguments *)
val make_fail : typ -> typed_term
val make_let : (id * id list * typed_term) list -> typed_term -> typed_term
val make_lets : (id * id list * typed_term) list -> typed_term -> typed_term
val make_letrecs : (id * id list * typed_term) list -> typed_term -> typed_term
val make_letrec : (id * id list * typed_term) list -> typed_term -> typed_term
val make_let_f : rec_flag -> (id * id list * typed_term) list -> typed_term -> typed_term
val make_lets_f : (rec_flag * (id * id list * typed_term)) list -> typed_term -> typed_term
val make_fun : id -> typed_term -> typed_term
val make_funs : id list -> typed_term -> typed_term
val make_not : typed_term -> typed_term
val make_and : typed_term -> typed_term -> typed_term
val make_ands : typed_term list -> typed_term
val make_or : typed_term -> typed_term -> typed_term
val make_ors : typed_term list -> typed_term
val make_add : typed_term -> typed_term -> typed_term
val make_sub : typed_term -> typed_term -> typed_term
val make_mul : typed_term -> typed_term -> typed_term
val make_neg : typed_term -> typed_term
val make_if : typed_term -> typed_term -> typed_term -> typed_term
val make_br : typed_term -> typed_term -> typed_term
val make_eq : typed_term -> typed_term -> typed_term
val make_eq_dec : typed_term -> typed_term -> typed_term
val make_neq : typed_term -> typed_term -> typed_term
val make_lt : typed_term -> typed_term -> typed_term
val make_gt : typed_term -> typed_term -> typed_term
val make_leq : typed_term -> typed_term -> typed_term
val make_geq : typed_term -> typed_term -> typed_term
val make_fst : typed_term -> typed_term
val make_snd : typed_term -> typed_term
val make_pair : typed_term -> typed_term -> typed_term
val make_tuple : typed_term list -> typed_term
val make_nil : typ -> typed_term
val make_nil2 : typ -> typed_term
val make_cons : typed_term -> typed_term -> typed_term
val make_match : typed_term -> (typed_pattern * typed_term * typed_term) list -> typed_term
val make_single_match : ?total:bool -> typed_term -> typed_pattern -> typed_term -> typed_term
val make_seq : typed_term -> typed_term -> typed_term
val make_ignore : typed_term -> typed_term
val make_assert : typed_term -> typed_term
val make_assume : typed_term -> typed_term -> typed_term
val make_label : ?label:string -> Syntax.info -> Syntax.typed_term -> Syntax.typed_term
val make_pany : typ -> typed_pattern
val make_pvar : id -> typed_pattern
val make_pconst : typed_term -> typed_pattern
val make_ppair : typed_pattern -> typed_pattern -> typed_pattern
val make_pnil : typ -> typed_pattern
val make_pnil2 : typ -> typed_pattern
val make_pcons : typed_pattern -> typed_pattern -> typed_pattern
val make_imply : typed_term -> typed_term -> typed_term
val none_flag : typed_term
val some_flag : typed_term
val make_none : typ -> typed_term
val make_some : typed_term -> typed_term
val make_is_none : typed_term -> typed_term
val make_is_some : typed_term -> typed_term
val make_get_val : typed_term -> typed_term
val make_tuple : typed_term list -> typed_term
val make_tuple' : typed_term list -> typed_term
val make_proj : int -> typed_term -> typed_term
val make_ref : typed_term -> typed_term
val make_deref : typed_term -> typed_term
val make_setref : typed_term -> typed_term -> typed_term
val make_construct : string -> typed_term list -> typ -> typed_term
val make_record : (string * typed_term) list -> typ -> typed_term
val make_field : typed_term -> string -> typed_term
val make_raise : typed_term -> typ -> typed_term
val make_trywith : typed_term -> id -> (typed_pattern * typed_term * typed_term) list -> typed_term
val make_length : typed_term -> typed_term
val new_var_of_term : typed_term -> id


(** {6 Term destructor} *)
val is_none : typed_term -> bool
val decomp_some : typed_term -> typed_term option
val decomp_is_none : typed_term -> typed_term option
val decomp_get_val : typed_term -> typed_term option
val decomp_funs : typed_term -> id list * typed_term
val decomp_lets : typed_term -> (rec_flag * (id * id list * typed_term) list) list * typed_term
val decomp_var : typed_term -> id option
val decomp_bexp : typed_term -> typed_term list
val decomp_prog : typed_term -> (rec_flag * (id * id list * typed_term) list) list * typed_term
val decomp_list : typed_term -> typed_term list option
val var_of_term : typed_term -> id
val int_of_term : typed_term -> int
val bool_of_term : typed_term -> bool
val pair_of_term : typed_term -> typed_term * typed_term
val tuple_of_term : typed_term -> typed_term list
val list_of_term : typed_term -> typed_term list
val get_opt_typ : typ -> typ
val opt_typ : typ -> typ
val is_base_var : id -> bool
val is_fun_var : id -> bool
val is_list_literal : typed_term -> bool


(** {6 Misc} *)
val subst : id -> typed_term -> typed_term -> typed_term
val subst_int : int -> typed_term -> typed_term -> typed_term
val subst_map : (id * typed_term) list -> typed_term -> typed_term
val subst_type : id -> typed_term -> typ -> typ
val subst_type_var : id -> id -> typ -> typ
val subst_var : id -> id -> typed_term -> typed_term
val subst_data_type : string -> typ -> typ -> typ
val subst_data_type_term : string -> typ -> typed_term -> typed_term
val get_int : typed_term -> int list
val get_args : typ -> id list
val get_argvars : typ -> id list
val get_argtyps : typ -> typ list
val get_top_funs : typed_term -> id list
val get_top_rec_funs : typed_term -> id list
val get_vars_pat : typed_pattern -> id list
val get_fv : ?cmp:(id -> id -> bool) -> typed_term -> id list
val arg_num : typ -> int
val subst_rev : typed_term -> id -> typed_term -> typed_term
val replace_term : typed_term -> typed_term -> typed_term -> typed_term
val max_pat_num : typed_term -> int
val is_value : typed_term -> bool
val is_simple_aexp : typed_term -> bool
val is_simple_bexp : typed_term -> bool
val depends : typed_term -> id -> bool
val is_poly_typ : typ -> bool
val is_id_unique : typed_term -> bool
val occur : id -> typ -> bool
val has_no_effect : typed_term -> bool
val same_term : typed_term -> typed_term -> bool
val same_term' : typed_term -> typed_term -> bool
val var_name_of_term : typed_term -> string
val var_of_term : typed_term -> id
val make_term : typ -> typed_term
val col_same_term : typed_term -> typed_term -> typed_term list
val col_info_id : typed_term -> id list
val is_bottom_def : Syntax.rec_flag -> id -> id list -> Syntax.typed_term -> bool
val merge_typ : typ -> typ -> typ
val get_last_definition : typed_term -> id option
val get_body : typed_term -> typed_term
val count_occurrence : id -> typed_term -> int
val add_attr : attr -> typed_term -> typed_term
val add_comment : string -> typed_term -> typed_term
val add_id : int -> typed_term -> typed_term
val remove_attr : attr -> typed_term -> typed_term
val get_bound_variables : typed_term -> id list
val get_id : typed_term -> int
val get_id_option : typed_term -> int option
val get_id_map : typed_term -> (int, typed_term) Hashtbl.t
val from_fpat_term : Fpat.Term.t -> typed_term
val from_fpat_formula : Fpat.Formula.t -> typed_term
val unfold_data_type : typ -> typ
val fold_data_type : typ -> typ
