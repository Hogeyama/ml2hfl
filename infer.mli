
(** Dependent type inferece *)

exception Untypable

type pid
type pred = Pred of pid * Syntax.t list 
type rty =
    RTunit of int
  | RTint of (Syntax.t -> pred)
  | RTbool of (Syntax.t -> pred)
  | RTifun of (Syntax.t -> pred) * (Syntax.t -> rty)
  | RTbfun of (Syntax.t -> pred) * (Syntax.t-> rty)
  | RTfun of rty list * rty
  | RTlist of (Syntax.t -> pred)
  | RTlfun of (Syntax.t -> pred) * (Syntax.t -> rty)

val cgen_flag : bool ref
(** This should be set to [false] if we want to propagate conditions on free variables eagerly. *)

val test : (Syntax.ident * Syntax.typ) list -> Syntax.ident ->
  (Syntax.ident * (Syntax.ident list * Syntax.t)) list ->
  Syntax.node list list -> Syntax.t option ->
  (Syntax.ident * rty list) list * (pid * (Syntax.ident list * Syntax.t)) list
(** [test tdefs s defs traces pred] で反例 [traces] に対応するSHPに型を付ける．
    [tdefs] は入力プログラムに与えられた固定の依存型？
    [s] はプログラムのメイン．
    [defs] は関数定義．
    [traces] は反例のリスト．
    [pred] は... (TODO: 説明追加) D-Or の規則に対応する検証をするためのもの．

    [s] および [defs] の body には変数束縛や関数抽象が入ってはならない．
    また，[s] および [defs] の body の型は unit型でなければならない．
*)

val print_rty : rty -> unit
