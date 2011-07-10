
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

val set_datatype_cvc3 : ?cout:out_channel -> Syntax.typed_term -> unit

val equiv : (CEGAR_syntax.var * 'a CEGAR_type.t) list -> CEGAR_syntax.t list -> CEGAR_syntax.t -> CEGAR_syntax.t -> bool
(** [equiv cond t1 t2] で，[cond],[t1] |= [t2] かつ [cond],[t1] |= [t2] かどうかを調べる *)

val check : (CEGAR_syntax.var * 'a CEGAR_type.t) list -> CEGAR_syntax.t list -> CEGAR_syntax.t -> bool
(** [check cond t] で，[cond] |= [t] かどうかを調べる *)

val checksat : (CEGAR_syntax.var * 'a CEGAR_type.t) list -> CEGAR_syntax.t -> bool
(** [checksat t] で，[t] が充足可能かどうかを調べる *)

val interpolation : (CEGAR_syntax.var * 'a CEGAR_type.t) list -> CEGAR_syntax.t list -> CEGAR_syntax.t list -> CEGAR_syntax.t
(** [interpolation ts1 ts2] で，[ts1],[ts2] それぞれの conjunction の interpolant を求める．*)

val get_solution : (CEGAR_syntax.var * 'a CEGAR_type.t) list -> CEGAR_syntax.t -> string list
(** [get_solution constr t] で制約 [constr] の解を求める．
    [constr] が充足不能の場合はどうなるかわからない．
*)





