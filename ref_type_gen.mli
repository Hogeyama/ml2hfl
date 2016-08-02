open Syntax
open Ref_type

val generate_check :
  (t * (id * typ Id.t list * typed_term)) list ->
  (t * (id * typ Id.t list * typed_term)) list ->
  id ->
  t ->
  (t * (id * typ Id.t list * typed_term)) list *
  (t * (id * typ Id.t list * typed_term)) list *
  typed_term

val generate :
  (t * (id * typ Id.t list * typed_term)) list ->
  (t * (id * typ Id.t list * typed_term)) list ->
  t ->
  (t * (id * typ Id.t list * typed_term)) list *
  (t * (id * typ Id.t list * typed_term)) list *
  typed_term
(*
val generate_check_cps : id -> t -> typed_term
val generate_cps : t -> typed_term
 *)
