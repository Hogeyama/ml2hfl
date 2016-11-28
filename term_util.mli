open Syntax

val dummy_var : id
val abst_var : id
val abst_var_int : id
val abst_var_bool : id

val typ_result : typ
val typ_event : typ
val typ_event' : typ
val typ_event_cps : typ
val typ_exn : typ

val make_attr : ?attrs:attr list -> term list -> attr list

(** {6 Term constructor} *)
val unit_term : term
val true_term : term
val false_term : term
val cps_result : term
val fail_term : term
val fail_term_cps : term
val randint_term : term
val randbool_unit_term : term
val randint_unit_term : term
val fail_unit_term : term
val make_bool : bool -> term
val make_bottom : typ -> term
val make_event : string -> term
val make_event_unit : string -> term
val make_event_cps : string -> term
val make_var : id -> term
val make_int : int -> term
val make_string : string -> term
val make_randvalue : typ -> term
val make_randvalue_unit : typ -> term
val make_randvalue_cps : typ -> term
val make_randint_cps : bool -> term
val make_app : term -> term list -> term
val make_app_raw : term -> term list -> term (** Does not merge arguments *)
val make_fail : typ -> term
val make_let : (id * id list * term) list -> term -> term
val make_lets : (id * id list * term) list -> term -> term
val make_letrecs : (id * id list * term) list -> term -> term
val make_letrec : (id * id list * term) list -> term -> term
val make_let_f : rec_flag -> (id * id list * term) list -> term -> term
val make_lets_f : (rec_flag * (id * id list * term)) list -> term -> term
val make_fun : id -> term -> term
val make_funs : id list -> term -> term
val make_not : term -> term
val make_and : term -> term -> term
val make_ands : term list -> term
val make_or : term -> term -> term
val make_ors : term list -> term
val make_add : term -> term -> term
val make_sub : term -> term -> term
val make_mul : term -> term -> term
val make_neg : term -> term
val make_if : term -> term -> term -> term
val make_br : term -> term -> term
val make_eq : term -> term -> term
val make_eq_dec : term -> term -> term
val make_neq : term -> term -> term
val make_lt : term -> term -> term
val make_gt : term -> term -> term
val make_leq : term -> term -> term
val make_geq : term -> term -> term
val make_fst : term -> term
val make_snd : term -> term
val make_pair : term -> term -> term
val make_tuple : term list -> term
val make_nil : typ -> term
val make_nil2 : typ -> term
val make_cons : term -> term -> term
val make_match : term -> (pattern * term * term) list -> term
val make_single_match : ?total:bool -> term -> pattern -> term -> term
val make_seq : term -> term -> term
val make_ignore : term -> term
val make_assert : term -> term
val make_assume : term -> term -> term
val make_label : ?label:string -> Syntax.info -> Syntax.term -> Syntax.term
val make_pany : typ -> pattern
val make_pvar : id -> pattern
val make_pconst : term -> pattern
val make_ppair : pattern -> pattern -> pattern
val make_pnil : typ -> pattern
val make_pnil2 : typ -> pattern
val make_pcons : pattern -> pattern -> pattern
val make_imply : term -> term -> term
val none_flag : term
val some_flag : term
val make_none : typ -> term
val make_some : term -> term
val make_is_none : term -> term
val make_is_some : term -> term
val make_get_val : term -> term
val make_tuple : term list -> term
val make_tuple' : term list -> term
val make_proj : int -> term -> term
val make_ref : term -> term
val make_deref : term -> term
val make_setref : term -> term -> term
val make_construct : string -> term list -> typ -> term
val make_record : (string * term) list -> typ -> term
val make_field : term -> string -> term
val make_raise : term -> typ -> term
val make_trywith : term -> id -> (pattern * term * term) list -> term
val make_trywith_simple : term -> term -> term
val make_length_var : typ -> id
val make_length : term -> term
val new_var_of_term : term -> id


(** {6 Term destructor} *)
val is_none : term -> bool
val decomp_some : term -> term option
val decomp_is_none : term -> term option
val decomp_get_val : term -> term option
val decomp_funs : term -> id list * term
val decomp_lets : term -> (rec_flag * (id * id list * term) list) list * term
val decomp_var : term -> id option
val decomp_bexp : term -> term list
val decomp_prog : term -> (rec_flag * (id * id list * term) list) list * term
val decomp_list : term -> term list option
val var_of_term : term -> id
val int_of_term : term -> int
val bool_of_term : term -> bool
val pair_of_term : term -> term * term
val tuple_of_term : term -> term list
val list_of_term : term -> term list
val get_opt_typ : typ -> typ
val opt_typ : typ -> typ
val is_base_var : id -> bool
val is_fun_var : id -> bool
val is_list_literal : term -> bool
val is_var : term -> bool


(** {6 Misc} *)
val subst : id -> term -> term -> term
val subst_int : int -> term -> term -> term
val subst_map : (id * term) list -> term -> term
val subst_type : id -> term -> typ -> typ
val subst_type_var : id -> id -> typ -> typ
val subst_var : id -> id -> term -> term
val subst_data_type : string -> typ -> typ -> typ
val subst_data_type_term : string -> typ -> term -> term
val get_int : term -> int list
val get_args : typ -> id list
val get_argvars : typ -> id list
val get_argtyps : typ -> typ list
val get_top_funs : term -> id list
val get_top_rec_funs : term -> id list
val get_vars_pat : pattern -> id list
val get_fv : ?cmp:(id -> id -> bool) -> term -> id list
val arg_num : typ -> int
val subst_rev : term -> id -> term -> term
val replace_term : term -> term -> term -> term
val max_pat_num : term -> int
val is_value : term -> bool
val is_simple_aexp : term -> bool
val is_simple_bexp : term -> bool
val depends : term -> id -> bool
val is_poly_typ : typ -> bool
val is_id_unique : term -> bool
val occur : id -> typ -> bool
val has_no_effect : term -> bool
val same_term : term -> term -> bool
val same_term' : term -> term -> bool
val var_name_of_term : term -> string
val var_of_term : term -> id
val make_term : typ -> term
val col_same_term : term -> term -> term list
val col_info_id : term -> id list
val is_bottom_def : Syntax.rec_flag -> id -> id list -> Syntax.term -> bool
val merge_typ : typ -> typ -> typ
val get_last_definition : term -> id option
val get_body : term -> term
val count_occurrence : id -> term -> int
val add_attr : attr -> term -> term
val add_attrs : attr list -> term -> term
val add_comment : string -> term -> term
val add_id : int -> term -> term
val col_id : term -> int list
val replace_id : int -> int -> term -> term
val remove_attr : attr -> term -> term
val get_bound_variables : term -> id list
val get_id : term -> int
val get_id_option : term -> int option
val get_id_map : term -> (int, term) Hashtbl.t
val from_fpat_term : Fpat.Term.t -> term
val from_fpat_formula : Fpat.Formula.t -> term
val unfold_data_type : typ -> typ
val fold_data_type : typ -> typ
val find_exn_typ : term -> typ option
val col_typ_var : term -> typ option ref list
