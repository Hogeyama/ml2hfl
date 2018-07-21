open Util
open Combinator

(** Rays *)

type t = { origin: Point3.t; direction: Vector3.t }

let make origin direction = { origin = origin; direction = direction }

let point_at ray t = Point3.translate ray.origin (Vector3.scale t ray.direction)
