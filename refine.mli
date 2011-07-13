
(** CEGAR *)

exception CannotRefute

val refine : int list list -> CEGAR_syntax.prog -> CEGAR_syntax.prog
(** [refine traces t] で，反例のリスト [traces] を用いて [t] の述語発見を行う．
*)
(*
val add_preds_ : (Syntax.id * Syntax.typ) list -> Syntax.typed_term -> Syntax.typed_term
(** [add_preds_ tdefs t] で， [t] 中の各変数 [xi] に述語 [ti] を追加する (tdefs = \[x1,t1;...\])． *)

val remove_preds : Syntax.typed_term -> Syntax.typed_term
(** [remove_preds t] で， [t] 中の述語を削除する *)
*)
