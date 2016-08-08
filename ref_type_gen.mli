open Ref_type

val generate_check :
  Syntax.typ option ->
  ?make_fail:(Syntax.typ -> Syntax.typed_term) ->
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list ->
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list ->
  Syntax.id ->
  t ->
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list *
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list *
  Syntax.typed_term

val generate :
  Syntax.typ option ->
  ?make_fail:(Syntax.typ -> Syntax.typed_term) ->
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list ->
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list ->
  t ->
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list *
  (t * (Syntax.id * Syntax.typ Id.t list * Syntax.typed_term)) list *
  Syntax.typed_term
