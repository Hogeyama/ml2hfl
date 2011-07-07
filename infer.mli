
(** Dependent type inferece *)

exception Untypable

type pid
type pred = Pred of pid * Syntax.typed_term list 
type rty =
    RTunit of int
  | RTint of (Syntax.typed_term -> pred)
  | RTbool of (Syntax.typed_term -> pred)
  | RTifun of (Syntax.typed_term -> pred) * (Syntax.typed_term -> rty)
  | RTbfun of (Syntax.typed_term -> pred) * (Syntax.typed_term-> rty)
  | RTfun of rty list * rty
  | RTlist of (Syntax.typed_term -> pred)
  | RTlfun of (Syntax.typed_term -> pred) * (Syntax.typed_term -> rty)

val cgen_flag : bool ref
(** This should be set to [false] if we want to propagate conditions on free variables eagerly. *)

val test : (Syntax.id * Syntax.typ) list -> Syntax.id ->
  (Syntax.id * (Syntax.id list * Syntax.typed_term)) list ->
  Syntax.node list list -> Syntax.typed_term option ->
  (Syntax.id * rty list) list * (pid * (Syntax.id list * Syntax.typed_term)) list
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
