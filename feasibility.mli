
(** Checking feasibility *)

(**
今の実装はcall-by-valueでのfeasibility.
評価戦略をCPS変換と合わせる必要がある．
*)

(*
val get_prefix: Syntax.node list -> (Syntax.id * ('a * Syntax.typed_term)) list -> Syntax.typed_term -> Syntax.node list
(** [get_prefix ce defs s] で，実行不能となる [ce] の最小のprefixを求める．
    実行可能なパスであった場合，例外 [Feasible] を投げる．
    [defs] は関数定義．
    [s] はプログラムのメイン．
*)
*)
type result =
  | Feasible of int list
  | Infeasible of CEGAR_syntax.ce


val check: CEGAR_syntax.ce -> CEGAR_syntax.prog -> result

(** [check ce defs s] で，反例 [ce] が実際にあり得るパスかどうかをチェックする．
    [defs] は関数定義．
    [s] はプログラムのメイン．
*)

val trans_ce: CEGAR_syntax.ce -> CEGAR_syntax.prog -> bool list
val print_ce_reduction: CEGAR_syntax.ce -> CEGAR_syntax.prog -> unit

(*
val check_int: Syntax.node list -> (Syntax.id * ('a * Syntax.typed_term)) list -> Syntax.typed_term -> Syntax.typed_term * Syntax.node list
(** ??? *)
*)
