
(** 外部ツール (CVC3, CSIsat) のラッパー *)

(**
TODO:
 - cvc3 で 同じコンストラクタを使えるようにする
*)

exception Satisfiable

val open_cvc3 : unit -> unit
(** cvc3 を開き，初期化する．*)

val close_cvc3 : unit -> unit
(** cvc3 を閉じる．*)

val set_datatype_cvc3 : ?cout:out_channel -> Syntax.t -> unit

val equiv : Syntax.t list -> Syntax.t -> Syntax.t -> bool
(** [equiv cond t1 t2] で，[cond],[t1] |= [t2] かつ [cond],[t1] |= [t2] かどうかを調べる *)

val check : Syntax.t list -> Syntax.t -> bool
(** [check cond t] で，[cond] |= [t] かどうかを調べる *)

val checksat : Syntax.t -> bool
(** [checksat t] で，[t] が充足可能かどうかを調べる *)

val interpolation : Syntax.t list -> Syntax.t list -> Syntax.t
(** [interpolation ts1 ts2] で，[ts1],[ts2] それぞれの conjunction の interpolant を求める．*)

val congruent : Syntax.t list -> Syntax.typ -> Syntax.typ -> bool
(** [congruent cond typ1 typ2] で型（述語）の意味が同じかどうかを調べる *) 

val get_solution : Syntax.t -> Syntax.t -> string list
(** [get_solution constr t] で制約 [constr] の解を求める．
    [constr] が充足不能の場合はどうなるかわからない．
*)

val simplify_exp : Syntax.t -> Syntax.t

val simplify_bool_exp : bool -> Syntax.t -> Syntax.t



