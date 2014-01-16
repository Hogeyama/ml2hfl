
(** CEGAR *)

exception CannotRefute

val refine :
  string list ->
  (Fpat.Idnt.t -> bool) ->
  CEGAR_syntax.ce -> CEGAR_syntax.ce list ->
  CEGAR_syntax.prog ->
  (CEGAR_syntax.var * CEGAR_syntax.typ) list * CEGAR_syntax.prog
(** [refine prefix traces t] で，反例のリスト [traces] を用いて [t] の述語発見を行う．
*)

val add_preds : CEGAR_syntax.env -> CEGAR_syntax.prog -> CEGAR_syntax.prog

(*
val remove_preds : Syntax.typed_term -> Syntax.typed_term
(** [remove_preds t] で， [t] 中の述語を削除する *)
*)

exception PostCondition of (Fpat.Idnt.t * Fpat.MLType.t) list * Fpat.Formula.t * Fpat.Formula.t

val progWithExparam : CEGAR_syntax.prog ref

val refine_rank_fun : CEGAR_syntax.ce -> CEGAR_syntax.prog -> unit
