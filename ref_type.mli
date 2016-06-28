module S = Syntax
module U = Term_util
val debug : unit -> bool
type base = Unit | Bool | Int | Abst of string
type t =
  | Base of base * S.id * S.typed_term
  | Fun of S.id * t * t
  | Tuple of (S.id * t) list
  | Inter of S.typ * t list
  | Union of S.typ * t list
  | ExtArg of S.id * t * t
  | List of S.id * S.typed_term * S.id * S.typed_term * t
module Env : Ext.Env.ENV with type key := S.id with type value := t
type env = Env.t

val _Inter : S.typ -> t list -> t
val _Union : S.typ -> t list -> t
val _ExtArg : S.id -> t -> t -> t
val decomp_base : t -> (base * S.id * S.typed_term) option
val decomp_fun : t -> (S.id * t * t) option
val decomp_list : t -> (S.id * S.typed_term * S.id * S.typed_term * t) option
val decomp_inter : t -> t list
val is_base : t -> bool
val is_fun : t -> bool
val is_list : t -> bool
val print_base : Format.formatter -> base -> unit
val occur : Syntax.id -> t -> bool
val print : Format.formatter -> t -> unit
val decomp_funs : int -> t -> (S.id * t) list * (S.id * t) list * t
val arg_num : t -> int
val map_pred : (S.typed_term -> S.typed_term) -> t -> t
val subst : Syntax.id -> Syntax.typed_term -> t -> t
val subst_var : Syntax.id -> Syntax.id -> t -> t
val subst_rev : Syntax.typed_term -> Syntax.id -> t -> t
val replace_term : S.typed_term -> Syntax.typed_term -> t -> t
val rename : t -> t
val from_simple : S.typ -> t
val to_simple : t -> S.typ
val to_abst_typ_base : base -> 'a Type.t
val to_abst_typ : t -> Syntax.typ
val set_base_var : S.id -> t -> t
val copy_fun_arg_to_base : t -> t
val same : t -> t -> bool
val has_no_predicate : t -> bool
val make_rand : Syntax.typ -> Syntax.typed_term
val generate_check :
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list ->
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list ->
  Syntax.id ->
  t ->
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list *
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list *
  Syntax.typed_term
val generate_simple : Syntax.typ -> Syntax.typed_term
val generate :
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list ->
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list ->
  t ->
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list *
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list *
  S.typed_term
val conv : S.typed_term -> Fpat.Formula.t
val is_sat : S.typed_term -> bool
val is_valid : S.typed_term -> bool
val implies : S.typed_term list -> S.typed_term -> bool
val simplify_pred : S.typed_term -> S.typed_term
val flatten : t -> t
val simplify_typs : (t list -> t) -> (Syntax.typed_term list -> S.typed_term) -> t list -> t
val simplify : t -> t
(*
val from_fpat_const : Fpat.TypConst.t -> base
val from_fpat : Fpat.RefType.t -> t
 *)
val make_strongest : S.typ -> t
val make_weakest : S.typ -> t
val typ_result : t

val union : S.typ -> t list -> t
