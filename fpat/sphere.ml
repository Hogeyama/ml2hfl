open Util
open Combinator

(** Spheres *)

type t = { center: Point3.t; radius: float }

let make center radius = assert(radius > 0.0); { center = center; radius = radius }

let calc_normal sphere p = Vector3.normalize (Point3.from_to sphere.center p)

let rt sphere attribute =
  object
    method raytrace ray =
      (* calculate the discriminant *)
      let center_to_eye = Point3.from_to sphere.center ray.Ray.origin in
      let b = Vector3.dot ray.Ray.direction center_to_eye in
      let c = (Vector3.dot center_to_eye center_to_eye) -. sphere.radius *. sphere.radius in
      let discriminant = b *. b -. c in
      if discriminant <= 0.0 then
        [] (* the ray and the sphere have no intersection *)
      else
        let sq_disc = sqrt discriminant in
        let distance1 = -.b -. sq_disc in
        let distance2 = -.b +. sq_disc in
        if distance2 < 0.0 then
          []
        else if distance1 < 0.0 then
          let intersection2 = Ray.point_at ray distance2 in
          let normal2 = calc_normal sphere intersection2 in
          [RTPoint3.make distance2 intersection2 normal2 false attribute]
        else
          let intersection1 = Ray.point_at ray distance1 in
          let normal1 = calc_normal sphere intersection1 in
          let intersection2 = Ray.point_at ray distance2 in
          let normal2 = calc_normal sphere intersection2 in
          [RTPoint3.make distance1 intersection1 normal1 true attribute;
           RTPoint3.make distance2 intersection2 normal2 false attribute]
  end
