open Util
open Combinator

(** Planes *)

type t =
  { center: Point3.t;
    normal: Vector3.t }

let make center normal =
  { center = center;
    normal = Vector3.normalize normal }

let rt plane attribute =
  object
    method raytrace ray =
      let v_n = Vector3.dot ray.Ray.direction plane.normal in
      if v_n = 0.0 then
        [] (* the ray and the plane are parallel *)
      else
        let distance = (Vector3.dot (Point3.from_to ray.Ray.origin plane.center) plane.normal) /. v_n in
        if distance < 0.0 then
          []
        else
          let intersection = Ray.point_at ray distance in
          let normal = plane.normal in
          [RTPoint3.make distance intersection normal (v_n < 0.0) attribute]
  end
