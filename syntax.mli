
(** *)

type label = Read | Write | Close
type binop = Eq | Lt | Gt | Leq | Geq | And | Or | Add | Sub | Mult
type typ = typed_term Type.t
and id = typ Id.t
and typed_term = {desc:term; typ:typ}
and term =
    Unit
  | True
  | False
  | Unknown
  | Int of int
  | NInt of id
  | RandInt of typed_term option
  | RandValue of typ * typed_term option
  | Var of id
  | Fun of id * typed_term
  | App of typed_term * typed_term list
  | If of typed_term * typed_term * typed_term
  | Branch of typed_term * typed_term
  | Let of Flag.rec_flag * id * id list * typed_term * typed_term
  | BinOp of binop * typed_term * typed_term
  | Not of typed_term
  | Label of bool * typed_term
  | LabelInt of int * typed_term
  | Event of string
  | Record of (string * (Flag.mutable_flag * typed_term)) list
  | Proj of int * string * Flag.mutable_flag * typed_term
  | SetField of int option * int * string * Flag.mutable_flag * typed_term * typed_term
  | Nil
  | Cons of typed_term * typed_term
  | Constr of string * typed_term list
  | Match of typed_term * (typed_pattern * typed_term option * typed_term) list
  | Raise of typed_term
  | TryWith of typed_term * typed_term
  | Pair of typed_term * typed_term
  | Fst of typed_term
  | Snd of typed_term
  | Variant of typed_term (* for abstraction of datatype *)
  | Bottom

and type_kind =
    KAbstract
  | KVariant of (string * typ list) list
  | KRecord of (string * (Flag.mutable_flag * typ)) list
and pred = term
and typed_pattern = {pat_desc:pattern; pat_typ:typ}
and pattern =
    PVar of id
  | PConst of typed_term
  | PConstruct of string * typed_pattern list
  | PNil
  | PCons of typed_pattern * typed_pattern
  | PPair of typed_pattern * typed_pattern
  | PRecord of (int * (string * Flag.mutable_flag * typed_pattern)) list
  | POr of typed_pattern * typed_pattern
type node = BrNode | LabNode of bool | FailNode | EventNode of string | PatNode of int

type literal = Cond of typed_term | Pred of (id * int * id * typed_term list)

exception Feasible of typed_term
exception Infeasible


val dummy_var : id
val abst_var : id
val abst_list_var : id

val typ_event : typ
val typ_excep : typ ref

val unit_term : typed_term
val true_term : typed_term
val false_term : typed_term
val fail_term : typed_term
val make_bottom : typ -> typed_term
val make_event : string -> typed_term
val make_var : id -> typed_term
val make_int : int -> typed_term
val make_app : typed_term -> typed_term list -> typed_term
val make_loop : typ -> typed_term
val make_fail : typ -> typed_term
val make_let : id -> id list -> typed_term -> typed_term -> typed_term
val make_letrec : id -> id list -> typed_term -> typed_term -> typed_term
val make_let_f : Flag.rec_flag -> id -> id list -> typed_term -> typed_term -> typed_term
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
val make_pair : typed_term -> typed_term -> typed_term
val make_nil : typ -> typed_term
val make_variant : typed_term -> typed_term
val make_match : typed_term -> (typed_pattern * typed_term option * typed_term) list -> typed_term
val make_loop : typ -> typed_term
val make_pvar : id -> typed_pattern
val make_pconst : typed_term -> typed_pattern
val make_pnil : typ -> typed_pattern
val make_pcons : typed_pattern -> typed_pattern -> typed_pattern


val subst : id -> typed_term -> typed_term -> typed_term
val subst_int : int -> typed_term -> typed_term -> typed_term
val subst_term : (id * typed_term) list -> typed_term -> typed_term
val subst_type : id -> typed_term -> typ -> typ
val get_nint : typed_term -> id list
val get_int : typed_term -> int list
val get_fv : typed_term -> id list
val get_fv2 : typed_term -> id list
val get_args : typ -> id list
val get_argvars : typ -> id list
val get_argtyps : typ -> typ list
val merge_let_fun : typed_term -> typed_term
(** [let f ... = fun x -> t] や [let f ... = let g x = t in g] を [let f ... x = t] に *)
val imply : typed_term -> typed_term -> typed_term
val and_list : typed_term list -> typed_term
val lift : typed_term -> (id * (id list * typed_term)) list * typed_term
(** [lift t] で，[t] をlambda-lift する．the definitions of let expressions must be side-effect free *)

val canonize : typed_term -> typed_term
val part_eval : typed_term -> typed_term
val add_string : string -> typed_term -> typed_term
val remove_unused : typed_term -> typed_term

val eval : typed_term -> typed_term
val eta_expand : typed_term -> typed_term
val normalize_bool_exp : typed_term -> typed_term
val get_and_list : typed_term -> typed_term list
val merge_geq_leq : typed_term -> typed_term
val set_target : typed_term -> typed_term

val max_pat_num : typed_term -> int
val max_label_num : typed_term -> int
val is_external : id -> bool
val init_rand_int : typed_term -> typed_term
val print_ce : node list -> typed_term -> unit
val copy_poly_funs : typed_term -> typed_term
(** CPS と評価順序を合わせる必要あり *)
val trans_let : typed_term -> typed_term
(** returns a term whose definitions of let expressions are side-effect free *)
val is_value : typed_term -> bool

(** {6 Printing} *)

val print_typ : Format.formatter -> typ -> unit
val print_id : Format.formatter -> id -> unit
val print_ids : Format.formatter -> id list -> unit
val print_id_typ : Format.formatter -> id -> unit
val print_ids_typ : Format.formatter -> id list -> unit
val print_termlist : int -> bool -> Format.formatter -> typed_term list -> unit
val string_of_ident : id -> string
val string_of_term : typed_term -> string
val string_of_node : node -> string
val print_pattern : Format.formatter -> typed_pattern -> unit
val print_constr : Format.formatter -> literal -> unit
val print_constr_list : Format.formatter -> literal list -> unit
val pp_print_typ : Format.formatter -> typ -> unit
(** Same as [print_typ] *)

val pp_print_term : Format.formatter -> typed_term -> unit
val pp_print_term_typ : Format.formatter -> typed_term -> unit
val print_defs : Format.formatter -> (id * (id list * typed_term)) list -> unit

val print_term : bool -> Format.formatter -> typed_term -> unit
val print_term' : bool -> Format.formatter -> typed_term -> unit

