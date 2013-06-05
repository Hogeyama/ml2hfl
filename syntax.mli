
(** Syntax of intermediate language *)

type label = Read | Write | Close
type binop = Eq | Lt | Gt | Leq | Geq | And | Or | Add | Sub | Mult
type typ = typed_term Type.t
and id = typ Id.t
and typed_term = {desc:term; typ:typ}
and const = (* only base type constants *)
    Unit
  | True
  | False
  | Int of int
  | Char of char
  | String of string
  | Float of string
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
and term =
    Const of const
  | Unknown
  | RandInt of bool (** true denotes CPS-term *)
  | RandValue of typ * bool (** true denotes CPS-term *)
  | Var of id
  | Fun of id * typed_term
  | App of typed_term * typed_term list
  | If of typed_term * typed_term * typed_term
  | Branch of typed_term * typed_term
  | Let of rec_flag * (id * id list * typed_term) list * typed_term
  | BinOp of binop * typed_term * typed_term
  | Not of typed_term
  | Event of string * bool
  | Record of (string * (mutable_flag * typed_term)) list
  | Proj of int * string * mutable_flag * typed_term
  | SetField of int option * int * string * mutable_flag * typed_term * typed_term
  | Nil
  | Cons of typed_term * typed_term
  | Constr of string * typed_term list
  | Match of typed_term * (typed_pattern * typed_term * typed_term) list
  | Raise of typed_term
  | TryWith of typed_term * typed_term
  | Pair of typed_term * typed_term
  | Fst of typed_term
  | Snd of typed_term
  | Bottom
  | Label of info * typed_term

and info =
    InfoInt of int
  | InfoString of string
  | InfoId of id
  | InfoTerm of typed_term

and rec_flag = Nonrecursive | Recursive
and mutable_flag = Immutable | Mutable


and type_kind =
    KAbstract
  | KVariant of (string * typ list) list
  | KRecord of (string * (mutable_flag * typ)) list
and pred = term
and typed_pattern = {pat_desc:pattern; pat_typ:typ}
and pattern =
    PAny
  | PVar of id
  | PAlias of typed_pattern * id
  | PConst of typed_term
  | PConstruct of string * typed_pattern list
  | PNil
  | PCons of typed_pattern * typed_pattern
  | PPair of typed_pattern * typed_pattern
  | PRecord of (int * (string * mutable_flag * typed_pattern)) list
  | POr of typed_pattern * typed_pattern
type node = BrNode | LabNode of bool | FailNode | EventNode of string | PatNode of int

type literal = Cond of typed_term | Pred of (id * int * id * typed_term list)

exception Feasible of typed_term
exception Infeasible


val dummy_var : id
val abst_var : id
val abst_var_int : id
val abst_var_bool : id
val length_var : id

val typ_event : typ
val typ_event_cps : typ
val typ_excep : typ ref
val typ_abst : typ

(** {6 Term constructor} *)
val unit_term : typed_term
val true_term : typed_term
val false_term : typed_term
val fail_term : typed_term
val randint_term : typed_term
val randbool_unit_term : typed_term
val randint_unit_term : typed_term
val abst_term : typed_term
val make_abst : typ -> typed_term
val make_bottom : typ -> typed_term
val make_event : string -> typed_term
val make_event_cps : string -> typed_term
val make_var : id -> typed_term
val make_int : int -> typed_term
val make_randint_cps : typ -> typed_term
val make_app : typed_term -> typed_term list -> typed_term
val make_loop : typ -> typed_term
val make_fail : typ -> typed_term
val make_let : (id * id list * typed_term) list -> typed_term -> typed_term
val make_lets : (id * id list * typed_term) list -> typed_term -> typed_term
val make_letrec : (id * id list * typed_term) list -> typed_term -> typed_term
val make_let_f : rec_flag -> (id * id list * typed_term) list -> typed_term -> typed_term
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
val make_assume : typed_term -> typed_term -> typed_term
val make_label : info -> typed_term -> typed_term
val make_pany : typ -> typed_pattern
val make_pvar : id -> typed_pattern
val make_pconst : typed_term -> typed_pattern
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
val get_nint : typed_term -> id list
val get_int : typed_term -> int list
val get_fv : ?cmp:(id -> id -> int) -> typed_term -> id list
val get_args : typ -> id list
val get_argvars : typ -> id list
val get_argtyps : typ -> typ list
val arg_num : typ -> int
val get_vars_pat : typed_pattern -> id list

val max_pat_num : typed_term -> int
val max_label_num : typed_term -> int
val is_parameter : id -> bool
val is_value : typed_term -> bool
val get_top_funs : typed_term -> id list


(** {6 Printing} *)

val print_typ : Format.formatter -> typ -> unit
val print_id : Format.formatter -> id -> unit
val print_ids : Format.formatter -> id list -> unit
val print_id_typ : Format.formatter -> id -> unit
val print_ids_typ : Format.formatter -> id list -> unit
val print_termlist : int -> bool -> Format.formatter -> typed_term list -> unit
val string_of_node : node -> string
val print_pattern : Format.formatter -> typed_pattern -> unit
val print_constr : Format.formatter -> literal -> unit
val print_constr_list : Format.formatter -> literal list -> unit
val pp_print_typ : Format.formatter -> typ -> unit
(** Same as [print_typ] *)

val print_const : Format.formatter -> const -> unit
val print_term : bool -> Format.formatter -> typed_term -> unit
val print_term' : Format.formatter -> typed_term -> unit
val pp_print_term : Format.formatter -> typed_term -> unit
val pp_print_term' : Format.formatter -> typed_term -> unit
(** Same as [print_term'] *)
val pp_print_term_typ : Format.formatter -> typed_term -> unit
val print_defs : Format.formatter -> (id * (id list * typed_term)) list -> unit
