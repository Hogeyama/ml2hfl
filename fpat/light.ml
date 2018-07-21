open Util
open Combinator
open Color

(** Lights *)

type light = Point3.t -> Vector3.t * RGBFFF.t

let parallel col dir p =
  let vec_in = Vector3.normalize (Vector3.neg dir) in
  vec_in, col

let point col pos p =
  let vec_in = Vector3.normalize (Point3.from_to p pos) in
  vec_in, col
