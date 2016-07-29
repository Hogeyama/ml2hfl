type 'a t =
    TUnit
  | TBool
  | TInt
  | TVar of 'a t option ref
  | TFun of 'a t Id.t * 'a t
  | TFuns of 'a t Id.t list * 'a t
  | TList of 'a t
  | TTuple of 'a t Id.t list
  | TData of string
  | TRef of 'a t
  | TOption of 'a t
  | TPred of 'a t Id.t * 'a list
  | TVariant of (string * 'a t list) list
  | TRecord of (string * (mutable_flag * 'a t)) list
  | Type of (string * 'a t) list * string
and mutable_flag = Immutable | Mutable

exception CannotUnify

val _TFun : 'a t Id.t -> 'a t -> 'a t

val print :
  ?occur:('a t Id.t -> 'a t -> bool) ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val print_typ_init : Format.formatter -> 'a t -> unit

val is_fun_typ : 'a t -> bool
val is_base_typ : 'a t -> bool
val can_unify : 'a t -> 'a t -> bool
val data_occurs : string -> 'a t -> bool
val same_shape : 'a t -> 'a t -> bool
val has_pred : 'a t -> bool
val is_mutable_record : 'a t -> bool

val typ_unknown : 'a t
val elim_tpred : 'a t -> 'a t
val tfuns_to_tfun : 'a t -> 'a t
val elim_tpred_all : 'a t -> 'a t
val decomp_tfun : 'a t -> 'a t Id.t list * 'a t
val decomp_tfuns : 'a t -> 'a t Id.t list * 'a t
val flatten : 'a t -> 'a t
val unify : 'a t -> 'a t -> unit
val copy : 'a t -> 'a t
val app_typ : 'a t -> 'b list -> 'a t
val to_id_string : 'a t -> string
val order : 'a t -> int
val arity : 'a t -> int

val tuple_num : 'a t -> int option
val proj_typ : int -> 'a t -> 'a t
val fst_typ : 'a t -> 'a t
val snd_typ : 'a t -> 'a t
val ref_typ : 'a t -> 'a t
val list_typ : 'a t -> 'a t
val option_typ : 'a t -> 'a t
val arg_var : 'a t -> 'a t Id.t
val result_typ : 'a t -> 'a t
val decomp_ttuple : 'a t -> 'a t list
val decomp_trecord : 'a t -> (string * (mutable_flag * 'a t)) list
val get_free_data_name : 'a t -> string list
