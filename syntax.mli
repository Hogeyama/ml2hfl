
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
  | CPS_result
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

exception Feasible of typed_term
exception Infeasible


val occur : id -> typ -> bool
val get_vars_pat : typed_pattern -> id list
val get_fv : ?cmp:(id -> id -> int) -> typed_term -> id list

(** {6 Printing} *)

val print_typ : Format.formatter -> typ -> unit
val print_id : Format.formatter -> id -> unit
val print_ids : Format.formatter -> id list -> unit
val print_id_typ : Format.formatter -> id -> unit
val print_ids_typ : Format.formatter -> id list -> unit
val print_termlist : int -> bool -> Format.formatter -> typed_term list -> unit
val string_of_const : const -> string
val string_of_binop : binop -> string
val string_of_typ : typ -> string
val string_of_node : node -> string
val print_pattern : Format.formatter -> typed_pattern -> unit
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
