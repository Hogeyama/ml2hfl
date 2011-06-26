
(** Alpha-renaming *)

val alpha : Syntax.t -> Syntax.t
(** [alpha t] で，[t] 中の変数の [id] を fresh なものにする *)
