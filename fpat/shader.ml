open Util
open Combinator
open Color

(** Shaders *)

type shader = Vector3.t -> Vector3.t -> Vector3.t -> RGBFFF.t

let pi = 4.0 *. atan 1.0

(* kd = rd /. pi *)
let lambert kd normal vin vout =
  let vin_n = Vector3.dot vin normal in
  RGBFFF.scale (max 0.0 vin_n) kd

(* rejection sampling *)
let rec lambert_sample normal vout =
  let vrand = Vector3.make (Random.float 2.0 -. 1.0) (Random.float 2.0 -. 1.0) (Random.float 2.0 -. 1.0) in
  if Vector3.dot vrand vrand <= 1.0 then
    Vector3.normalize (if Vector3.dot vrand normal < 0.0 then Vector3.neg vrand else vrand)
  else
    lambert_sample normal vout

let phong ks n normal vin vout =
  let vreflect = Vector3.reflect normal vin in
  let vreflect_vout = Vector3.dot vreflect vout in
  RGBFFF.scale ((max 0.0 vreflect_vout) ** n) ks

let composite sh1 sh2 normal vin vout =
  RGBFFF.add (sh1 normal vin vout) (sh2 normal vin vout)

let ideal_reflector kr normal vin vout:RGBFFF.t = kr

let ideal_reflector_sample normal vout = Vector3.reflect normal vout

let ideal_refractor kt normal vin vout:RGBFFF.t = kt

let ideal_refractor_sample n2_div_n1 normal vout =
  let f n1_div_n2' normal' cos1 =
    let cos2sq = 1.0 -. n1_div_n2' *. n1_div_n2' *. (1.0 -. cos1 *. cos1) in
    if cos2sq < 0.0 then
      (* Total Reflection *)
      ideal_reflector_sample normal' vout
    else
      Vector3.sub
        (Vector3.scale (n1_div_n2' *. cos1 -. (sqrt cos2sq)) normal')
        (Vector3.scale n1_div_n2' vout) in
  let n_vout = Vector3.dot normal vout in
  if n_vout >= 0.0 then
    f (1.0 /. n2_div_n1) normal n_vout
  else
    f n2_div_n1 (Vector3.neg normal) (-.n_vout)


(*let fresnel n2_div_n1 n_vin n_vout =
  let t1 = (n2_div_n1 *. n_vin -. n_vout) /. (n2_div_n1 *. n_vin +. n_vout) in
  let t2 = (n2_div_n1 *. n_vout -. n_vin) /. (n2_div_n1 *. n_vout +. n_vin) in
  0.5 *. (t1 *. t1 +. t2 *. t2)

let fresnel_refractor kt nr ng nb normal vin vout =
  let f nr' ng' nb' n_vin' n_vout' =
    RGBFFF.make
      (kt *. (1.0 -. (fresnel nr' n_vin' n_vout')))
      (kt *. (1.0 -. (fresnel ng' n_vin' n_vout')))
      (kt *. (1.0 -. (fresnel nb' n_vin' n_vout'))) in
  let n_vin = Vector3.dot normal vin in
  let n_vout = Vector3.dot normal vout in
  if n_vout >= 0.0 then
    if n_vin >= 0.0 then
      RGBFFF.black
    else
      f nr ng nb (-.n_vin) n_vout
  else
    if n_vin >= 0.0 then
      f (1.0 /. nr) (1.0 /. ng) (1.0 /. nb) n_vin (-.n_vout)
    else
      RGBFFF.black*)


(* Cook-Torrance‚Ìƒ‚ƒfƒ‹ *)
let geometry n_h n_vin n_vout vout_h =
  let t = 2.0 *. n_h /. vout_h in
  min 1.0 (min (t *. n_vin) (t *. n_vout))

let beckmann m n_h =
  let t1 = 1.0 /. (n_h *. n_h) in
  let t2 = 1.0 /. (m *. m) in
  0.25 *. t1 *. t1 *. t2 *. (exp ((1.0 -. t1) *. t2))

let fresnel_reflect n2_div_n1 vout_h =
  let c = vout_h in
  let g = sqrt (n2_div_n1 *. n2_div_n1 +. c *. c -. 1.0) in
  let t1 = (g -. c) /. (g +. c) in
  let t2 = (c *. (g +. c) -. 1.0) /. (c *. (g -. c) +. 1.0) in
  0.5 *. t1 *. t1 *. (1.0 +. t2 *. t2)

(* ks = rs /. pi *)
(* kd = rd /. pi *)
let cook_torrance kd ks m nr ng nb normal vin vout =
  let n_vin = Vector3.dot normal vin in
  let n_vout = Vector3.dot normal vout in
  let h = Vector3.normalize (Vector3.add vin vout) in
  let n_h = Vector3.dot normal h in
  let vout_h = Vector3.dot vout h in (* vin_h = vout_h *)
  RGBFFF.add
    (RGBFFF.scale
      ((beckmann m n_h) *. (geometry n_h n_vin n_vout vout_h) /. n_vout *. ks)
      (RGBFFF.make (fresnel_reflect nr vout_h) (fresnel_reflect ng vout_h) (fresnel_reflect nb vout_h)))
    (RGBFFF.scale (max 0.0 n_vin)  kd)

let cook_torrance_sample kd ks normal vout =
  let per = ks /. (RGBFFF.luminance kd +. ks) in
  if Random.float 1.0 < per then
    ideal_reflector_sample normal vout
  else
    lambert_sample normal vout

let diffuse_light ld normal vout = RGBFFF.scale (max 0.0 (Vector3.dot normal vout)) ld
