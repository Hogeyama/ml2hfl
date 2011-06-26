
(** Model checking *)

val model_check : Syntax.t -> Syntax.node list option
(** [model_check t] で boolean program [t] をモデル検査する．評価戦略は call-by-name．
    [t] が安全であれば [None] を返す．
    [t] が危険であれば [Some ce] の形で反例 [ce] を返す．
*)
