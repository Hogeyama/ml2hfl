
(** *)

type ident = {id:int; name:string; typ:typ}
and label = Read | Write | Close
and binop = Eq | Lt | Gt | Leq | Geq | And | Or | Add | Sub | Mult
and rec_flag = Nonrecursive | Recursive
and mutable_flag = Immutable | Mutable
and t =
    Unit
  | True
  | False
  | Unknown
  | Int of int
  | NInt of ident
  | RandInt of t option
  | RandValue of typ * t option
  | Var of ident
  | Fun of ident * t
  | App of t * t list
  | If of t * t * t
  | Branch of t * t
  | Let of rec_flag * ident * ident list * t * t
  | BinOp of binop * t * t
  | Not of t
  | Fail
  | Label of bool * t
  | LabelInt of int * t
  | Event of string
  | Record of bool * (string * (mutable_flag * t)) list (** 1st true denotes a tuple *)
  | Proj of int option * int * string * mutable_flag * t
  | SetField of int option * int * string * mutable_flag * t * t
  | Nil
  | Cons of t * t
  | Constr of string * t list
  | Match of t * t * ident * ident * t
  | Match_ of t * (pattern * t option * t) list
  | TryWith of t * (pattern * t option * t) list
  | Type_decl of (string * (typ list * type_kind)) list * t
  | Exception of string * typ list * t
and pred = t
and typ =
    TUnit
  | TBool
  | TAbsBool
  | TInt of pred list
  | TRInt of pred
  | TVar of typ option ref
  | TFun of (ident*typ) * typ
  | TList of typ * pred list
  | TConstr of string * bool
  | TVariant of (string * typ list) list
  | TRecord of bool * (string * (mutable_flag * typ)) list
  | TUnknown
and type_kind =
    KAbstract
  | KVariant of (string * typ list) list
  | KRecord of (string * (mutable_flag * typ)) list
and pattern =
    PVar of ident
  | PConst of t
  | PConstruct of string * pattern list
  | PNil
  | PCons of pattern * pattern
  | PRecord of bool * (int * (string * mutable_flag * pattern)) list
  | POr of pattern * pattern
type syntax = ML | TRecS | CVC3 | CSIsat
type node = BrNode | LabNode of bool | FailNode | EventNode of string | PatNode of int

type literal = Cond of t | Pred of (ident * int * ident * t list)

exception Feasible of t
exception Infeasible


val dummy_var : ident
val abst_var : ident
val abst_list_var : ident

val new_int : unit -> int
val get_counter : unit -> int
val set_counter : int -> unit
val new_var : string -> ident
val new_var' : string -> ident
val new_var_id : ident -> ident

val subst : ident -> t -> t -> t
val subst2 : ident -> t -> t -> t
val subst_int : int -> t -> t -> t
val subst_term : (ident * t) list -> t -> t
val subst_type : ident -> t -> typ -> typ
val subst_orig : ident -> t -> t -> t
val subst_type_orig : ident -> t -> typ -> typ
val fff : typ -> typ
val get_nint : t -> ident list
val get_int : t -> int list
val get_fv : t -> ident list
val get_fv2 : t -> ident list
val get_args : typ -> ident list
val get_argvars : typ -> ident list
val app2app : t -> t list -> t
val eval : t -> t
val fun2let : t -> t
val imply : t -> t -> t
val and_list : t list -> t
val lift : t -> (ident * (ident list * t)) list * t
(** [lift t] で，[t] を lambda-lift する．*)

val lift2 : t -> (ident * (ident list * t)) list * t
(** [lift2 t] で，[t] をlambda-lift する．
    [lift] とは違い，スコープに入っている変数を全て abstract する．
*)

val canonize : t -> t
val part_eval : t -> t
val expand : t -> typ -> t
val add_string_to_var : string -> ident -> ident
val add_string : string -> t -> t
val remove_unused : t -> t
val occurs : ident -> typ -> bool

val eta_expand : t -> t
val normalize_bool_exp : t -> t
val get_and_list : t -> t list
val merge_geq_leq : t -> t
val make_new_fun : ident -> t -> t
val set_target : t -> t
val is_value : t -> bool
val assoc_id : ident -> (ident * 'a) list -> 'a

val max_pat_num : t -> int
val max_label_num : t -> int
val get_decls : t -> (string * (typ list * type_kind)) list list
val is_external : ident -> bool
val init_rand_int : t -> t
val is_value : t -> bool
val print_ce : node list -> t -> unit
(** CPS と評価順序を合わせる必要あり *)
val same_ident : ident -> ident -> bool

(** {6 Printing} *)

val print_typ : Format.formatter -> typ -> unit
val print_preds : Format.formatter -> t list -> unit
val print_id : Format.formatter -> ident -> unit
val print_ids : Format.formatter -> ident list -> unit
val print_id_typ : Format.formatter -> ident -> unit
val print_ids_typ : Format.formatter -> ident list -> unit
val print_termlist : syntax -> int -> bool -> Format.formatter -> t list -> unit
val string_of_ident : ident -> string
val string_of_term : syntax -> t -> string
val string_of_node : node -> string
val print_hors : Format.formatter -> (ident * ident list * t) list * (int * string * int list) list -> unit
val print_term_fm : syntax -> bool -> Format.formatter -> t -> unit
val print_term : syntax -> bool -> t -> unit
val print_term_fm_break : syntax -> bool -> Format.formatter -> t -> unit
val print_term_break : syntax -> bool -> t -> unit
val print_constr : Format.formatter -> literal -> unit
val print_constr_list : Format.formatter -> literal list -> unit
val pp_print_typ : Format.formatter -> typ -> unit
(** Same as [print_typ] *)

val pp_print_term : Format.formatter -> t -> unit

