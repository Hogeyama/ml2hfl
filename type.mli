type base =
  | TUnit
  | TBool
  | TInt
  | TPrim of string
and 'a t =
  | TBase of base
  | TVar of 'a t option ref * int option
  | TFun of 'a t Id.t * 'a t
  | TFuns of 'a t Id.t list * 'a t (* Just for fair-termination *)
  | TTuple of 'a t Id.t list
  | TData of string
  | TVariant of bool * (string * 'a t list) list (** true means polymorphic variant *)
  | TRecord of (string * (mutable_flag * 'a t)) list
  | TApp of constr * 'a t list
  | TAttr of 'a attr list * 'a t
  | TModule of (string * 'a t) list
and mutable_flag = Immutable | Mutable
and constr =
  | TList
  | TRef
  | TOption
  | TArray
  | TLazy
and 'a attr =
  | TAPred of 'a t Id.t * 'a list
  | TAPureFun
  | TAEffect of effect
  | TAAssumePredTrue
and effect = EVar of int | ENone | ECont | EExcep

exception CannotUnify

val prim_base_types : string list

val is_fun_typ : 'a t -> bool
val is_base_typ : 'a t -> bool
val can_unify : 'a t -> 'a t -> bool
val data_occurs : string -> 'a t -> bool
val same_shape : 'a t -> 'a t -> bool
val has_pred : 'a t -> bool
val is_mutable_record : 'a t -> bool
val is_tvar : 'a t -> bool
val occurs : 'a t option ref -> 'a t -> bool

val typ_unknown : 'a t
val elim_tattr : 'a t -> 'a t
val tfuns_to_tfun : 'a t -> 'a t
val elim_tattr_all : 'a t -> 'a t
val flatten : 'a t -> 'a t
val unify : 'a t -> 'a t -> unit
val copy : 'a t -> 'a t
val app_typ : 'a t -> 'b list -> 'a t
val to_id_string : 'a t -> string
val order : 'a t -> int
val arity : 'a t -> int
val var_name_of : 'a t -> string
val remove_arg_at : int -> 'a t -> 'a t

(** {6 destructor} *)

val decomp_tfun : 'a t -> 'a t Id.t list * 'a t
val decomp_tfuns : 'a t -> 'a t Id.t list * 'a t
val tuple_num : 'a t -> int option
val proj_typ : int -> 'a t -> 'a t
val fst_typ : 'a t -> 'a t
val snd_typ : 'a t -> 'a t
val ref_typ : 'a t -> 'a t
val list_typ : 'a t -> 'a t
val option_typ : 'a t -> 'a t
val arg_var : 'a t -> 'a t Id.t
val result_typ : 'a t -> 'a t
val decomp_tvariant : 'a t -> bool * (string * 'a t list) list
val decomp_ttuple : 'a t -> 'a t list
val decomp_trecord : 'a t -> (string * (mutable_flag * 'a t)) list
val array_typ : 'a t -> 'a t
val decomp_tattr : 'a t -> 'a attr list * 'a t


(** {6 Type constructor} *)

val _TFun : 'a t Id.t -> 'a t -> 'a t
val _TAttr : 'a attr list -> 'a t -> 'a t
val pureTFun : ('a t Id.t * 'a t) -> 'a t
val make_ttuple : 'a t list -> 'a t
val make_ttuple' : 'a t list -> 'a t
val make_tpair : 'a t -> 'a t -> 'a t
val make_tfun : 'a t -> 'a t -> 'a t
val make_tlist : 'a t -> 'a t
val make_tref : 'a t -> 'a t
val make_toption : 'a t -> 'a t
val make_tarray : 'a t -> 'a t


(** {6 Printers} *)

val print :
  ?occur:('a t Id.t -> 'a t -> bool) ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val print_init : Format.formatter -> 'a t -> unit
val print_effect : Format.formatter -> effect -> unit

module Ty : sig
  val unit : 'a t
  val bool : 'a t
  val int : 'a t
  val fun_ : 'a t -> 'a t -> 'a t
  val funs : 'a t list -> 'a t -> 'a t
  val tuple : 'a t list -> 'a t
  val tuple' : 'a t list -> 'a t
  val pair : 'a t -> 'a t -> 'a t
  val list : 'a t -> 'a t
  val ref : 'a t -> 'a t
  val option : 'a t -> 'a t
  val array : 'a t -> 'a t
end
