open Util
open Combinator

(** Raytraced 3D float points *)

type 'a t =
  { distance: float;
    intersection: Point3.t;
    normal: Vector3.t;
    flag: bool; (* true:start false:end *)
    attribute: 'a }

let make distance intersection normal flag attribute =
  { distance = distance;
    intersection = intersection;
    normal = normal;
    flag = flag;
    attribute = attribute }

let (<) p1 p2 = p1.distance < p2.distance

let min p1 p2 = if p1 < p2 then p1 else p2

let max p1 p2 = if p1 < p2 then p2 else p1

let compare p1 p2 =
  if p1 < p2 then
    -1
  else if p2 < p1 then
    1
  else
    0
