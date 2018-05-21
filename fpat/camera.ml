open Util
open Combinator

(** Cameras *)

type t = { posture: Frame3.t; width: float; height: float; distance: float }
(*                    e2
 * (-w/2, h/2)――――↑――――( w/2, h/2)
 *      ｜            ｜            ｜
 *      ｜            ｜            ｜
 *      ＋――――――◎――――――→e1
 *      ｜            ｜            ｜
 *      ｜            ｜            ｜
 * (-w/2,-h/2)――――＋―――( w/2,-h/2)
 *)
(*                    e2
 *                    ↑
 *                    ｜
 *       O            ｜
 *    e3←――――――◎―――――――
 *             d      ｜
 *                    ｜
 *                    ｜
 *)

let make posture width height distance =
  { posture = posture; width = width; height = height; distance = distance }

(* x ∈ [-1, 1]  y ∈ [-1, 1] *)
let sample_ray x y camera =
  Ray.make camera.posture.Frame3.o
           (Vector3.normalize
             (Vector3.add
               (Vector3.scale (-.camera.distance) (Vector3.normalize camera.posture.Frame3.e3))
               (Vector3.add
                 (Vector3.scale (camera.width *. x /. 2.) (Vector3.normalize camera.posture.Frame3.e1))
                 (Vector3.scale (camera.height *. y /. 2.) (Vector3.normalize camera.posture.Frame3.e2)))))

let move_to p camera =
  let posture = camera.posture in
  { camera with posture = { posture with Frame3.o = p} }
