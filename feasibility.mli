
(** Checking feasibility *)

(**
今の実装はcall-by-valueでのfeasibility.
評価戦略をCPS変換と合わせる必要がある．

TODO:
 - term_of_pattern を POr を扱えるようにする（Syntax.typed_term list を返すように変更）
*)

(*
val get_prefix: Syntax.node list -> (Syntax.id * ('a * Syntax.typed_term)) list -> Syntax.typed_term -> Syntax.node list
(** [get_prefix ce defs s] で，実行不能となる [ce] の最小のprefixを求める．
    実行可能なパスであった場合，例外 [Feasible] を投げる．
    [defs] は関数定義．
    [s] はプログラムのメイン．
*)
*)
val check: int list -> CEGAR_syntax.prog -> ((string * CEGAR_syntax.t CEGAR_type.t) list * string list) option

(** [check ce defs s] で，反例 [ce] が実際にあり得るパスかどうかをチェックする．
    [defs] は関数定義．
    [s] はプログラムのメイン．
*)
(*
val check_int: Syntax.node list -> (Syntax.id * ('a * Syntax.typed_term)) list -> Syntax.typed_term -> Syntax.typed_term * Syntax.node list
(** ??? *)
*)
