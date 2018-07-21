open Util
open Combinator

(** 4x4 float matrices *)

type t = { m11: float; m12: float; m13: float; m14: float;
           m21: float; m22: float; m23: float; m24: float;
           m31: float; m32: float; m33: float; m34: float;
           m41: float; m42: float; m43: float; m44: float }

let make m11 m12 m13 m14
         m21 m22 m23 m24
         m31 m32 m33 m34
         m41 m42 m43 m44 = { m11 = m11; m12 = m12; m13 = m13; m14 = m14;
                             m21 = m21; m22 = m22; m23 = m23; m24 = m24;
                             m31 = m31; m32 = m32; m33 = m33; m34 = m34;
                             m41 = m41; m42 = m42; m43 = m43; m44 = m44 }

let id = make 1.0 0.0 0.0 0.0
              0.0 1.0 0.0 0.0
              0.0 0.0 1.0 0.0
              0.0 0.0 0.0 1.0

let add m1 m2 = make (m1.m11 +. m2.m11) (m1.m12 +. m2.m12) (m1.m13 +. m2.m13) (m1.m14 +. m2.m14)
                     (m1.m21 +. m2.m21) (m1.m22 +. m2.m22) (m1.m23 +. m2.m23) (m1.m24 +. m2.m24)
                     (m1.m31 +. m2.m31) (m1.m32 +. m2.m32) (m1.m33 +. m2.m33) (m1.m34 +. m2.m34)
                     (m1.m41 +. m2.m41) (m1.m42 +. m2.m42) (m1.m43 +. m2.m43) (m1.m44 +. m2.m44)

let sub m1 m2 = make (m1.m11 -. m2.m11) (m1.m12 -. m2.m12) (m1.m13 -. m2.m13) (m1.m14 -. m2.m14)
                     (m1.m21 -. m2.m21) (m1.m22 -. m2.m22) (m1.m23 -. m2.m23) (m1.m24 -. m2.m24)
                     (m1.m31 -. m2.m31) (m1.m32 -. m2.m32) (m1.m33 -. m2.m33) (m1.m34 -. m2.m34)
                     (m1.m41 -. m2.m41) (m1.m42 -. m2.m42) (m1.m43 -. m2.m43) (m1.m44 -. m2.m44)

let scale s m = make (s *. m.m11) (s *. m.m12) (s *. m.m13) (s *. m.m14)
                     (s *. m.m21) (s *. m.m22) (s *. m.m23) (s *. m.m24)
                     (s *. m.m31) (s *. m.m32) (s *. m.m33) (s *. m.m34)
                     (s *. m.m41) (s *. m.m42) (s *. m.m43) (s *. m.m44)

let mul m1 m2 = make (m1.m11 *. m2.m11 +. m1.m12 *. m2.m21 +. m1.m13 *. m2.m31 +. m1.m14 *. m2.m41)
                     (m1.m11 *. m2.m12 +. m1.m12 *. m2.m22 +. m1.m13 *. m2.m32 +. m1.m14 *. m2.m42)
                     (m1.m11 *. m2.m13 +. m1.m12 *. m2.m23 +. m1.m13 *. m2.m33 +. m1.m14 *. m2.m43)
                     (m1.m11 *. m2.m14 +. m1.m12 *. m2.m24 +. m1.m13 *. m2.m34 +. m1.m14 *. m2.m44)

                     (m1.m21 *. m2.m11 +. m1.m22 *. m2.m21 +. m1.m23 *. m2.m31 +. m1.m24 *. m2.m41)
                     (m1.m21 *. m2.m12 +. m1.m22 *. m2.m22 +. m1.m23 *. m2.m32 +. m1.m24 *. m2.m42)
                     (m1.m21 *. m2.m13 +. m1.m22 *. m2.m23 +. m1.m23 *. m2.m33 +. m1.m24 *. m2.m43)
                     (m1.m21 *. m2.m14 +. m1.m22 *. m2.m24 +. m1.m23 *. m2.m34 +. m1.m24 *. m2.m44)

                     (m1.m31 *. m2.m11 +. m1.m32 *. m2.m21 +. m1.m33 *. m2.m31 +. m1.m34 *. m2.m41)
                     (m1.m31 *. m2.m12 +. m1.m32 *. m2.m22 +. m1.m33 *. m2.m32 +. m1.m34 *. m2.m42)
                     (m1.m31 *. m2.m13 +. m1.m32 *. m2.m23 +. m1.m33 *. m2.m33 +. m1.m34 *. m2.m43)
                     (m1.m31 *. m2.m14 +. m1.m32 *. m2.m24 +. m1.m33 *. m2.m34 +. m1.m34 *. m2.m44)

                     (m1.m41 *. m2.m11 +. m1.m42 *. m2.m21 +. m1.m43 *. m2.m31 +. m1.m44 *. m2.m41)
                     (m1.m41 *. m2.m12 +. m1.m42 *. m2.m22 +. m1.m43 *. m2.m32 +. m1.m44 *. m2.m42)
                     (m1.m41 *. m2.m13 +. m1.m42 *. m2.m23 +. m1.m43 *. m2.m33 +. m1.m44 *. m2.m43)
                     (m1.m41 *. m2.m14 +. m1.m42 *. m2.m24 +. m1.m43 *. m2.m34 +. m1.m44 *. m2.m44)

let transpose m = make m.m11 m.m21 m.m31 m.m41
                       m.m12 m.m22 m.m32 m.m42
                       m.m13 m.m23 m.m33 m.m43
                       m.m14 m.m24 m.m34 m.m44

let det m = (m.m11 *. m.m22 -. m.m12 *. m.m21) *. (m.m33 *. m.m44 -. m.m34 *. m.m43)
         -. (m.m11 *. m.m23 -. m.m13 *. m.m21) *. (m.m32 *. m.m44 -. m.m34 *. m.m42)
         +. (m.m11 *. m.m24 -. m.m14 *. m.m21) *. (m.m32 *. m.m43 -. m.m33 *. m.m42)
         +. (m.m12 *. m.m23 -. m.m13 *. m.m22) *. (m.m31 *. m.m44 -. m.m34 *. m.m41)
         -. (m.m12 *. m.m24 -. m.m14 *. m.m22) *. (m.m31 *. m.m43 -. m.m33 *. m.m41)
         +. (m.m13 *. m.m24 -. m.m14 *. m.m23) *. (m.m31 *. m.m42 -. m.m32 *. m.m41)

let inv m =
  let t = (det m) in
  if t = 0.0 then
    failwith ""
  else
    let s = 1.0 /. t in
    make (s *. (m.m22 *. (m.m33 *. m.m44 -. m.m34 *. m.m43) +. m.m23 *. (m.m34 *. m.m42 -. m.m32 *. m.m44) +. m.m24 *. (m.m32 *. m.m43 -. m.m33 *. m.m42)))
         (s *. (m.m32 *. (m.m13 *. m.m44 -. m.m14 *. m.m43) +. m.m33 *. (m.m14 *. m.m42 -. m.m12 *. m.m44) +. m.m34 *. (m.m12 *. m.m43 -. m.m13 *. m.m42)))
         (s *. (m.m42 *. (m.m13 *. m.m24 -. m.m14 *. m.m23) +. m.m43 *. (m.m14 *. m.m22 -. m.m12 *. m.m24) +. m.m44 *. (m.m12 *. m.m23 -. m.m13 *. m.m22)))
         (s *. (m.m12 *. (m.m24 *. m.m33 -. m.m23 *. m.m34) +. m.m13 *. (m.m22 *. m.m34 -. m.m24 *. m.m32) +. m.m14 *. (m.m23 *. m.m32 -. m.m22 *. m.m33)))
         
         (s *. (m.m23 *. (m.m31 *. m.m44 -. m.m34 *. m.m41) +. m.m24 *. (m.m33 *. m.m41 -. m.m31 *. m.m43) +. m.m21 *. (m.m34 *. m.m43 -. m.m33 *. m.m44)))
         (s *. (m.m33 *. (m.m11 *. m.m44 -. m.m14 *. m.m41) +. m.m34 *. (m.m13 *. m.m41 -. m.m11 *. m.m43) +. m.m31 *. (m.m14 *. m.m43 -. m.m13 *. m.m44)))
         (s *. (m.m43 *. (m.m11 *. m.m24 -. m.m14 *. m.m21) +. m.m44 *. (m.m13 *. m.m21 -. m.m11 *. m.m23) +. m.m41 *. (m.m14 *. m.m23 -. m.m13 *. m.m24)))
         (s *. (m.m13 *. (m.m24 *. m.m31 -. m.m21 *. m.m34) +. m.m14 *. (m.m21 *. m.m33 -. m.m23 *. m.m31) +. m.m11 *. (m.m23 *. m.m34 -. m.m24 *. m.m33)))
         
         (s *. (m.m24 *. (m.m31 *. m.m42 -. m.m32 *. m.m41) +. m.m21 *. (m.m32 *. m.m44 -. m.m34 *. m.m42) +. m.m22 *. (m.m34 *. m.m41 -. m.m31 *. m.m44)))
         (s *. (m.m34 *. (m.m11 *. m.m42 -. m.m12 *. m.m41) +. m.m31 *. (m.m12 *. m.m44 -. m.m14 *. m.m42) +. m.m32 *. (m.m14 *. m.m41 -. m.m11 *. m.m44)))
         (s *. (m.m44 *. (m.m11 *. m.m22 -. m.m12 *. m.m21) +. m.m41 *. (m.m12 *. m.m24 -. m.m14 *. m.m22) +. m.m42 *. (m.m14 *. m.m21 -. m.m11 *. m.m24)))
         (s *. (m.m14 *. (m.m22 *. m.m31 -. m.m21 *. m.m32) +. m.m11 *. (m.m24 *. m.m32 -. m.m22 *. m.m34) +. m.m12 *. (m.m21 *. m.m34 -. m.m24 *. m.m31)))
         
         (s *. (m.m21 *. (m.m33 *. m.m42 -. m.m32 *. m.m43) +. m.m22 *. (m.m31 *. m.m43 -. m.m33 *. m.m41) +. m.m23 *. (m.m32 *. m.m41 -. m.m31 *. m.m42)))
         (s *. (m.m31 *. (m.m13 *. m.m42 -. m.m12 *. m.m43) +. m.m32 *. (m.m11 *. m.m43 -. m.m13 *. m.m41) +. m.m33 *. (m.m12 *. m.m41 -. m.m11 *. m.m42)))
         (s *. (m.m41 *. (m.m13 *. m.m22 -. m.m12 *. m.m23) +. m.m42 *. (m.m11 *. m.m23 -. m.m13 *. m.m21) +. m.m43 *. (m.m12 *. m.m21 -. m.m11 *. m.m22)))
         (s *. (m.m11 *. (m.m22 *. m.m33 -. m.m23 *. m.m32) +. m.m12 *. (m.m23 *. m.m31 -. m.m21 *. m.m33) +. m.m13 *. (m.m21 *. m.m32 -. m.m22 *. m.m31)))

let print_matrix ppf m =
  Format.fprintf
    ppf
    ("@[<v>|@[% f@ % f@ % f@ % f@]|@ |@[% f@ % f@ % f@ % f@]|@ |@[% f@ % f@ % f@ % f@]|@ |@[% f@ % f@ % f@ % f@]|@]")
    m.m11 m.m12 m.m13 m.m14
    m.m21 m.m22 m.m23 m.m24
    m.m31 m.m32 m.m33 m.m34
    m.m41 m.m42 m.m43 m.m44
