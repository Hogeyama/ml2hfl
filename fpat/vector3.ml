open Util
open Combinator

(** 3D float vectors *)

type t = { x: float; y: float; z: float }

let make x y z = { x = x; y = y; z = z }

let id = make 0.0 0.0 0.0

let e1 = make 1.0 0.0 0.0

let e2 = make 0.0 1.0 0.0

let e3 = make 0.0 0.0 1.0

let add v1 v2 = make (v1.x +. v2.x) (v1.y +. v2.y) (v1.z +. v2.z)

let sub v1 v2 = make (v1.x -. v2.x) (v1.y -. v2.y) (v1.z -. v2.z)

let neg v = make (-.v.x) (-.v.y) (-.v.z)

let scale s v = make (s *. v.x) (s *. v.y) (s *. v.z)

let dot v1 v2 = v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

let norm v = sqrt (dot v v)

let normalize v = scale (1.0 /. (norm v)) v

let interpolate v1 v2 t =
  make (v1.x +. t *. (v2.x -. v1.x)) (v1.y +. t *. (v2.y -. v1.y)) (v1.z +. t *. (v2.z -. v1.z))

let cross v1 v2 =
  make (v1.y *. v2.z -. v1.z *. v2.y) (v1.z *. v2.x -. v1.x *. v2.z) (v1.x *. v2.y -. v1.y *. v2.x)

let reflect n v =
  let t = 2.0 *. (dot n v) /. (dot n n) in
  make (t *. n.x -. v.x) (t *. n.y -. v.y) (t *. n.z -. v.z)

let mul (m:Matrix44.t) v = make (m.Matrix44.m11 *. v.x +. m.Matrix44.m12 *. v.y +. m.Matrix44.m13 *. v.z)
                                (m.Matrix44.m21 *. v.x +. m.Matrix44.m22 *. v.y +. m.Matrix44.m23 *. v.z)
                                (m.Matrix44.m31 *. v.x +. m.Matrix44.m32 *. v.y +. m.Matrix44.m33 *. v.z)

let print_vector ppf v = Format.fprintf ppf "(@[% f,@ % f,@ % f@])" v.x v.y v.z
