
(** CEGAR *)

val refine : (Syntax.ident * Syntax.typ) list -> Syntax.node list list -> Syntax.t -> Syntax.t
(** [refine tdefs traces t] で，反例のリスト [traces] を用いて [t] の述語発見を行う．
    [tdefs] は入力プログラムに与えられた固定の依存型？
    [t] はトップレベルのみに関数定義がある形でなければならない．
*)

val add_preds_ : (Syntax.ident * Syntax.typ) list -> Syntax.t -> Syntax.t
(** [add_preds_ tdefs t] で， [t] 中の各変数 [xi] に述語 [ti] を追加する (tdefs = \[x1,t1;...\])． *)

val remove_preds : Syntax.t -> Syntax.t
(** [remove_preds t] で， [t] 中の述語を削除する *)

