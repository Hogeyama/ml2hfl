
(** Predicate abstraction *)

(** 古い実装では次の点でCPSに依存している
- TRecSに渡すプログラムにタプルが入らないように
- f:int[]->unit のとき，f fail が f にならないように（副作用のある式を消さないように）
*)

val abstract : CEGAR_syntax.prog -> CEGAR_syntax.prog
(** [abstract t] で [t] の述語抽象を求める．
    [t] は CPS かつ型付きでなければならない．
*)

val abstract_mutable : Syntax.typed_term -> Syntax.typed_term

val abst_ext_funs : Syntax.typed_term -> Syntax.typed_term
