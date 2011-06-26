
(** Simple type inferece *)

exception CannotUnify

type key = KeyVar of int * string | KeyLabelResult of string | KeyTypeEntity of string

val in_env : key -> bool
val add_type_env : key -> Syntax.typ -> unit
val add_exc_env : string -> Syntax.typ list -> unit

val new_var : Syntax.ident -> Syntax.ident * Syntax.typ

val typing : Syntax.t -> Syntax.t
(** [typing t] で，[t] に型を付けたものを返す．
    型が付かない場合は [CannotUnify] を返す．
*)

val typing_defs : (Syntax.ident * (Syntax.ident list * Syntax.t)) list -> Syntax.t ->
           (Syntax.ident * (Syntax.ident list * Syntax.t)) list * Syntax.t

(** [typing_defs defs s] で，関数定義 [defs]，メイン [s] に型を付けたものを返す．
    型が付かない場合は [CannotUnify] を返す．
*)

val get_typ : Syntax.t -> Syntax.typ
(** [get_typ t] で，[t] の型を返す．
    ただし，[t] には既に型が付いている必要がある．
*)

val get_type_of_constr : string -> Syntax.typ
val get_constrs_from_type : Syntax.typ -> (string * Syntax.typ list) list

val copy_poly_funs : Syntax.t -> Syntax.t

(**/**)

val type_checking : Syntax.t -> unit
