
(** Alpha-renaming *)

val alpha : Syntax.typed_term -> Syntax.typed_term
(** [alpha t] で，[t] 中の変数の [id] を fresh なものにする *)
