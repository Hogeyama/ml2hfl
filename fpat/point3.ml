open Util
open Combinator

(** 3D float points *)

type t = { x: float; y: float; z: float }

let make x y z = { x = x; y = y; z = z }

let o = make 0.0 0.0 0.0

let translate p (v:Vector3.t) = make (p.x +. v.Vector3.x) (p.y +. v.Vector3.y) (p.z +. v.Vector3.z)

let from_to p1 p2 = Vector3.make (p2.x -. p1.x) (p2.y -. p1.y) (p2.z -. p1.z)

let interpolate p1 p2 t =
  make (p1.x +. t *. (p2.x -. p1.x)) (p1.y +. t *. (p2.y -. p1.y)) (p1.z +. t *. (p2.z -. p1.z))

let distance p1 p2 =
  sqrt ((p1.x -. p2.x) *. (p1.x -. p2.x) +. (p1.y -. p2.y) *. (p1.y -. p2.y) +. (p1.z -. p2.z) *. (p1.z -. p2.z))

let mul (m:Matrix44.t) p = make (m.Matrix44.m11 *. p.x +. m.Matrix44.m12 *. p.y +. m.Matrix44.m13 *. p.z +. m.Matrix44.m14)
                                (m.Matrix44.m21 *. p.x +. m.Matrix44.m22 *. p.y +. m.Matrix44.m23 *. p.z +. m.Matrix44.m24)
                                (m.Matrix44.m31 *. p.x +. m.Matrix44.m32 *. p.y +. m.Matrix44.m33 *. p.z +. m.Matrix44.m34)

let print_point ppf p = Format.fprintf ppf "(@[% f,@ % f,@ % f@])" p.x p.y p.z
