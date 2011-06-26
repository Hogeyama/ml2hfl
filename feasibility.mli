
(** Checking feasibility *)

(**
TODO:
 - term_of_pattern を POr を扱えるようにする（Syntax.t list を返すように変更）
*)


val get_prefix: Syntax.node list -> (Syntax.ident * ('a * Syntax.t)) list -> Syntax.t -> Syntax.node list
(** [get_prefix ce defs s] で，実行不能となる [ce] の最小のprefixを求める．
    実行可能なパスであった場合，例外 [Feasible] を投げる．
    [defs] は関数定義．
    [s] はプログラムのメイン．
*)

val check: Syntax.node list -> (Syntax.ident * ('a * Syntax.t)) list -> Syntax.t -> unit
(** [check ce defs s] で，反例 [ce] が実際にあり得るパスかどうかをチェックする．
    実行可能なパスであった場合，例外 [Feasible] を投げる．
    [defs] は関数定義．
    [s] はプログラムのメイン．
*)

val check_int: Syntax.node list -> (Syntax.ident * ('a * Syntax.t)) list -> Syntax.t -> Syntax.t * Syntax.node list
(** ??? *)
