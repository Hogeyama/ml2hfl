
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

val reopen_cvc3 : unit -> unit
(** equivalent to (close_cvc3 . open_cvc3) *)

val equiv : Syntax.typed_term list -> Syntax.typed_term -> Syntax.typed_term -> bool
(** [equiv cond t1 t2] で，[cond],[t1] |= [t2] かつ [cond],[t1] |= [t2] かどうかを調べる *)

val check : Syntax.typed_term list -> Syntax.typed_term -> bool
(** [check cond t] で，[cond] |= [t] かどうかを調べる *)

val checksat : Syntax.typed_term -> bool
(** [checksat t] で，[t] が充足可能かどうかを調べる *)

val interpolation : Syntax.typed_term list -> Syntax.typed_term list -> Syntax.typed_term
(** [interpolation ts1 ts2] で，[ts1],[ts2] それぞれの conjunction の interpolant を求める．*)

val congruent : Syntax.typed_term list -> Syntax.typ -> Syntax.typ -> bool
(** [congruent cond typ1 typ2] で型（述語）の意味が同じかどうかを調べる *) 

val get_solution : Syntax.typed_term -> Syntax.typed_term -> string list
(** [get_solution constr t] で制約 [constr] の解を求める．
    [constr] が充足不能の場合はどうなるかわからない．
*)

val simplify_exp : Syntax.typed_term -> Syntax.typed_term

val simplify_bool_exp : bool -> Syntax.typed_term -> Syntax.typed_term



