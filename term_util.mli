open Syntax

type trans =
  {mutable tr_term: typed_term -> typed_term;
   mutable tr_term_rec: typed_term -> typed_term;
   mutable tr_desc: term -> term;
   mutable tr_desc_rec: term -> term;
   mutable tr_typ: typ -> typ;
   mutable tr_typ_rec: typ -> typ;
   mutable tr_var: id -> id;
   mutable tr_var_rec: id -> id;
   mutable tr_pat: typed_pattern -> typed_pattern;
   mutable tr_pat_rec: typed_pattern -> typed_pattern;
   mutable tr_info: info -> info;
   mutable tr_info_rec: info -> info;
   mutable tr_const: const -> const;
   mutable tr_const_rec: const -> const}

type 'a trans2 =
  {mutable tr2_term: 'a -> typed_term -> typed_term;
   mutable tr2_term_rec: 'a -> typed_term -> typed_term;
   mutable tr2_desc: 'a -> term -> term;
   mutable tr2_desc_rec: 'a -> term -> term;
   mutable tr2_typ: 'a -> typ -> typ;
   mutable tr2_typ_rec: 'a -> typ -> typ;
   mutable tr2_var: 'a -> id -> id;
   mutable tr2_var_rec: 'a -> id -> id;
   mutable tr2_pat: 'a -> typed_pattern -> typed_pattern;
   mutable tr2_pat_rec: 'a -> typed_pattern -> typed_pattern;
   mutable tr2_info: 'a -> info -> info;
   mutable tr2_info_rec: 'a -> info -> info;
   mutable tr2_const: 'a -> const -> const;
   mutable tr2_const_rec: 'a -> const -> const}

type 'a col =
  {mutable col_term: typed_term -> 'a;
   mutable col_term_rec: typed_term -> 'a;
   mutable col_desc: term -> 'a;
   mutable col_desc_rec: term -> 'a;
   mutable col_typ: typ -> 'a;
   mutable col_typ_rec: typ -> 'a;
   mutable col_var: id -> 'a;
   mutable col_var_rec: id -> 'a;
   mutable col_pat: typed_pattern -> 'a;
   mutable col_pat_rec: typed_pattern -> 'a;
   mutable col_info: info -> 'a;
   mutable col_info_rec: info -> 'a;
   mutable col_const: const -> 'a;
   mutable col_const_rec: const -> 'a;
   mutable col_app: 'a -> 'a -> 'a;
   mutable col_empty: 'a}

val make_trans : unit -> trans
val make_trans2 : unit -> 'a trans2
val make_col : 'a -> ('a -> 'a -> 'a) -> 'a col

val dummy_var : id
val abst_var : id
val abst_var_int : id
val abst_var_bool : id
val length_var : id

val typ_result : typ
val typ_event : typ
val typ_event' : typ
val typ_event_cps : typ
val typ_excep : typ ref

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
val make_bottom : typ -> typed_term
val make_event : string -> typed_term
val make_event_cps : string -> typed_term
val make_var : id -> typed_term
val make_int : int -> typed_term
val make_randvalue : typ -> typed_term
val make_randvalue_cps : typ -> typed_term
val make_randint_cps : unit -> typed_term
val make_app : typed_term -> typed_term list -> typed_term
val make_loop : typ -> typed_term
val make_fail : typ -> typed_term
val make_let : (id * id list * typed_term) list -> typed_term -> typed_term
val make_lets : (id * id list * typed_term) list -> typed_term -> typed_term
val make_letrec : (id * id list * typed_term) list -> typed_term -> typed_term
val make_let_f : rec_flag -> (id * id list * typed_term) list -> typed_term -> typed_term
val make_lets_f : (rec_flag * (id * id list * typed_term)) list -> typed_term -> typed_term
val make_fun : id -> typed_term -> typed_term
val make_not : typed_term -> typed_term
val make_and : typed_term -> typed_term -> typed_term
val make_or : typed_term -> typed_term -> typed_term
val make_add : typed_term -> typed_term -> typed_term
val make_sub : typed_term -> typed_term -> typed_term
val make_mul : typed_term -> typed_term -> typed_term
val make_neg : typed_term -> typed_term
val make_if : typed_term -> typed_term -> typed_term -> typed_term
val make_branch : typed_term -> typed_term -> typed_term
val make_eq : typed_term -> typed_term -> typed_term
val make_neq : typed_term -> typed_term -> typed_term
val make_lt : typed_term -> typed_term -> typed_term
val make_gt : typed_term -> typed_term -> typed_term
val make_leq : typed_term -> typed_term -> typed_term
val make_geq : typed_term -> typed_term -> typed_term
val make_fst : typed_term -> typed_term
val make_snd : typed_term -> typed_term
val make_pair : ?s:string -> typed_term -> typed_term -> typed_term
val make_tuple : typed_term list -> typed_term
val make_nil : typ -> typed_term
val make_nil2 : typ -> typed_term
val make_cons : typed_term -> typed_term -> typed_term
val make_match : typed_term -> (typed_pattern * typed_term * typed_term) list -> typed_term
val make_single_match : typed_term -> typed_pattern -> typed_term -> typed_term
val make_loop : typ -> typed_term
val make_nth : int -> int -> typed_term -> typed_term
val make_seq : typed_term -> typed_term -> typed_term
val make_assert : typed_term -> typed_term
val make_assume : typed_term -> typed_term -> typed_term
val make_label : info -> typed_term -> typed_term
val make_pany : typ -> typed_pattern
val make_pvar : id -> typed_pattern
val make_pconst : typed_term -> typed_pattern
val make_ppair : typed_pattern -> typed_pattern -> typed_pattern
val make_pnil : typ -> typed_pattern
val make_pnil2 : typ -> typed_pattern
val make_pcons : typed_pattern -> typed_pattern -> typed_pattern
val imply : typed_term -> typed_term -> typed_term
val and_list : typed_term list -> typed_term
val get_typ_default : typ -> typed_term

(** {6 Term destructor} *)
val decomp_fun : typed_term -> id list * typed_term

(** {6 Misc} *)
val subst : id -> typed_term -> typed_term -> typed_term
val subst_int : int -> typed_term -> typed_term -> typed_term
val subst_map : (id * typed_term) list -> typed_term -> typed_term
val subst_type : id -> typed_term -> typ -> typ
val get_int : typed_term -> int list
val get_args : typ -> id list
val get_argvars : typ -> id list
val get_argtyps : typ -> typ list
val arg_num : typ -> int
val subst_rev : typed_term -> id -> typed_term -> typed_term
val replace_term : typed_term -> typed_term -> typed_term -> typed_term
val max_pat_num : typed_term -> int
val max_label_num : typed_term -> int
val is_parameter : id -> bool
val is_value : typed_term -> bool
val get_top_funs : typed_term -> id list
val occur : id -> typ -> bool
val get_vars_pat : typed_pattern -> id list
val get_fv : ?cmp:(id -> id -> int) -> typed_term -> id list
val has_no_effect : typed_term -> bool
