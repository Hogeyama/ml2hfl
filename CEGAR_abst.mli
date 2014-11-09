(** Predicate abstraction *)

exception NotRefined

(**
call-by-name に対応させる際，let x = fail; 1 in assert (x = x) が unsafe と判定されるようにしないといけない

古い実装では次の点でCPSに依存している
- TRecSに渡すプログラムにタプルが入らないように
- f:int[]->unit のとき，f fail が f にならないように（副作用のある式を消さないように）
*)

val abstract : CEGAR_syntax.var list -> CEGAR_syntax.var list -> ?top_funs:'a Id.t list -> CEGAR_syntax.prog -> CEGAR_syntax.var list * CEGAR_syntax.prog
(** [abstract orig_fun_list force t] で [t] の述語抽象を求める．
    入力は、引数を評価しても fail しないものとする
*)

val incr_wp_max : bool ref
