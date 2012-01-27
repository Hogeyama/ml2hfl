
(** CEGAR *)

exception CannotRefute

val refine :
  (CEGAR_syntax.var * int * int list) list ->
  CEGAR_syntax.ce -> CEGAR_syntax.ce ->
  CEGAR_syntax.prog ->
  (CEGAR_syntax.var * CEGAR_syntax.typ) list * CEGAR_syntax.prog
(** [refine prefix traces t] で，反例のリスト [traces] を用いて [t] の述語発見を行う．
*)

val add_preds : CEGAR_syntax.env -> CEGAR_syntax.env -> CEGAR_syntax.env

(*
val remove_preds : Syntax.typed_term -> Syntax.typed_term
(** [remove_preds t] で， [t] 中の述語を削除する *)
*)


