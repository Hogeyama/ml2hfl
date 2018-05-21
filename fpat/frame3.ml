open Util
open Combinator

(** 3D float frames *)

type t = { o: Point3.t; e1: Vector3.t; e2: Vector3.t; e3: Vector3.t }

let make o e1 e2 e3 = { o = o; e1 = e1; e2 = e2; e3 = e3 }

let cartesian = make Point3.o Vector3.e1 Vector3.e2 Vector3.e3
