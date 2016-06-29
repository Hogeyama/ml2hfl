
type base = Unit | Bool | Int | Abst of string
type t =
  | Base of base * Syntax.id * Syntax.typed_term
  | Fun of Syntax.id * t * t
  | Tuple of (Syntax.id * t) list
  | Inter of Syntax.typ * t list
  | Union of Syntax.typ * t list
  | ExtArg of Syntax.id * t * t
  | List of Syntax.id * Syntax.typed_term * Syntax.id * Syntax.typed_term * t
module Env : Ext.Env.ENV with type key := Syntax.id with type value := t
type env = Env.t


(** {6 Constructor} *)
val _Inter : Syntax.typ -> t list -> t
val _Union : Syntax.typ -> t list -> t
val _ExtArg : Syntax.id -> t -> t -> t
val typ_result : t


(** {6 Destructor} *)
val decomp_base : t -> (base * Syntax.id * Syntax.typed_term) option
val decomp_fun : t -> (Syntax.id * t * t) option
val decomp_list : t -> (Syntax.id * Syntax.typed_term * Syntax.id * Syntax.typed_term * t) option
val decomp_inter : t -> t list
val is_base : t -> bool
val is_fun : t -> bool
val is_list : t -> bool
val decomp_funs : int -> t -> (Syntax.id * t) list * (Syntax.id * t) list * t
val arg_num : t -> int

val inter : Syntax.typ -> t list -> t
val union : Syntax.typ -> t list -> t


(** {6 Transformation} *)
val simplify : t -> t
val remove_subtype : t list -> t list

(** {6 Converter} *)
val of_simple : Syntax.typ -> t
val to_simple : t -> Syntax.typ
val to_abst_typ_base : base -> 'a Type.t
val to_abst_typ : t -> Syntax.typ


(** {6 Printer} *)
val print_base : Format.formatter -> base -> unit
val print : Format.formatter -> t -> unit


(** {6 Generator} *)
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
  Syntax.typed_term
val make_strongest : Syntax.typ -> t
val make_weakest : Syntax.typ -> t


(** {6 Misc} *)
val map_pred : (Syntax.typed_term -> Syntax.typed_term) -> t -> t
val flatten : t -> t
val occur : Syntax.id -> t -> bool
val subst : Syntax.id -> Syntax.typed_term -> t -> t
val subst_var : Syntax.id -> Syntax.id -> t -> t
val subst_rev : Syntax.typed_term -> Syntax.id -> t -> t
val replace_term : Syntax.typed_term -> Syntax.typed_term -> t -> t
val rename : t -> t
val set_base_var : Syntax.id -> t -> t
val copy_fun_arg_to_base : t -> t
val same : t -> t -> bool
val has_no_predicate : t -> bool
