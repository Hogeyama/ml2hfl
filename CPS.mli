
(** CPS-transformation for source language *)

val trans : Syntax.typed_term -> Syntax.typed_term * (Syntax.id -> Ref_type.t -> Ref_type.t)

(*
val trans_simpl : Syntax.typed_term -> Syntax.typed_term
*)

val remove_pair : Syntax.typed_term -> Syntax.typed_term * (Syntax.id -> Ref_type.t -> Ref_type.t)
