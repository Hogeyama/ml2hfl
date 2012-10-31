
(** Model checking *)

val model_check : CEGAR_syntax.prog -> int -> int list option
(** [model_check n t] で boolean program [t] をモデル検査する．
    [n] は抽象化前のプログラムの関数定義の数．
    評価戦略は call-by-name．
    [t] が安全であれば [None] を返す．
    [t] が危険であれば [Some ce] の形で反例 [ce] を返す．
*)
