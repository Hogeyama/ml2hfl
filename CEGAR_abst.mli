(** Predicate abstraction *)

(**
call-by-name で対応させる際，let x = fail; 1 in assert (x = x) が unsafe と判定されるようにしないといけない

古い実装では次の点でCPSに依存している
- TRecSに渡すプログラムにタプルが入らないように
- f:int[]->unit のとき，f fail が f にならないように（副作用のある式を消さないように）
*)

val abstract : CEGAR_syntax.prog -> CEGAR_syntax.prog
(** [abstract t] で [t] の述語抽象を求める．
    入力は、引数を評価しても fail しないものとする
*)

val eval_abst_cbn : CEGAR_syntax.prog -> CEGAR_syntax.prog -> CEGAR_syntax.ce -> unit
