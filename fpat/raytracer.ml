open Util
open Combinator
open Csg
open Color

(** Raytracer *)

let max_path_length = 10
let max_sample = 100

let rec nearest out ps =
  match ps with
    [] ->
      None
  | p::ps ->
      if (out && p.RTPoint3.flag) || ((not out) && (not p.RTPoint3.flag)) then
        Some(p)
      else
        nearest out ps

let rec calc_rad robj lights env ray out n =
  if n > max_path_length then
    RGBFFF.black
  else
    match nearest out (robj#raytrace ray) with
      None ->
        env n
    | Some(p) ->
        let vout = Vector3.normalize (Vector3.neg ray.Ray.direction) in
        let (shader, sampler, emitter) = p.RTPoint3.attribute in
        let shader_n = shader p.RTPoint3.normal in
        (* 発光 *)
        let cemit = emitter p.RTPoint3.normal vout in
        (* 間接照明 *)
        (* 反射・屈折ベクトル *)
        let vin = sampler p.RTPoint3.normal vout in
        let cscatter_indirect =
          RGBFFF.mul
            (calc_rad
              robj
              lights
              env
              (Ray.make
                (p.RTPoint3.intersection)
                vin)
              (Vector3.dot vin p.RTPoint3.normal >= 0.0)
              (n + 1))
            (shader_n vin vout) in
        (* 直接照明 *)
        let cscatter_direct light =
          let (vin, clight) = light (p.RTPoint3.intersection) in
          let shadow_ray = Ray.make (p.RTPoint3.intersection) vin in
          if nearest out (robj#raytrace shadow_ray) = None then
              RGBFFF.mul clight (shader_n vin vout)
          else
              RGBFFF.black in
        List.fold_left
          (RGBFFF.add)
          (RGBFFF.add cemit cscatter_indirect)
          (List.map cscatter_direct lights)

let render robj lights env camera img graphics =
  let sw = 2.0 /. (float_of_int img.Image.width) in
  let sh = -2.0 /. (float_of_int img.Image.height) in
  for y = 0 to img.Image.height - 1 do
    for x = 0 to img.Image.width - 1 do
      let crad = ref RGBFFF.black in
      for i = 1 to max_sample do
        let ray = Camera.sample_ray
                    (sw *. (float_of_int x +. Random.float 1.0) -. 1.0)
                    (sh *. (float_of_int y +. Random.float 1.0) +. 1.0)
                    camera in
        crad := RGBFFF.add !crad (calc_rad robj lights env ray true 1)
      done;
      let color = RGBFFF.rgb888 (RGBFFF.scale (1.0 /. (float_of_int max_sample)) !crad) in
      Printf.printf "(%03d, %03d)\n" x y;
     (if graphics then
       (Graphics.set_color color;
        Graphics.plot x (img.Image.height - 1 - y)));
      (*draw_image (Image.caml_image img) 0 0;*)
      Image.set x y color img
    done
  done





let test () =
  let mat_white =
    (Shader.composite
       (Shader.lambert RGBFFF.white)
       (Shader.phong RGBFFF.gray 16.0)),
    Shader.lambert_sample,
    Shader.diffuse_light RGBFFF.black in
  let mat_light_red =
    (Shader.composite
      (Shader.lambert (RGBFFF.make 1.0 0.5 0.5))
      (Shader.phong RGBFFF.gray 16.0)),
    Shader.lambert_sample,
    Shader.diffuse_light RGBFFF.black in
  let mat_light_green =
    (Shader.composite
      (Shader.lambert (RGBFFF.make 0.5 1.0 0.5))
      (Shader.phong RGBFFF.gray 16.0)),
    Shader.lambert_sample,
    Shader.diffuse_light RGBFFF.black in
  let mat_light_blue =
    (Shader.composite
      (Shader.lambert (RGBFFF.make 0.5 0.5 1.0))
      (Shader.phong RGBFFF.gray 16.0)),
    Shader.lambert_sample,
    Shader.diffuse_light RGBFFF.black in
  let mat_light_gray =
    (Shader.composite
       (Shader.lambert RGBFFF.light_gray)
       (Shader.phong RGBFFF.gray 16.0)),
    Shader.lambert_sample,
    Shader.diffuse_light RGBFFF.black in
  let mat_red =
    (Shader.composite
       (Shader.lambert RGBFFF.red)
       (Shader.phong RGBFFF.gray 16.0)),
    Shader.lambert_sample,
    Shader.diffuse_light RGBFFF.black in
  let mat_blue =
    (Shader.composite
       (Shader.lambert RGBFFF.blue)
       (Shader.phong RGBFFF.gray 16.0)),
    Shader.lambert_sample,
    Shader.diffuse_light RGBFFF.black in
  let mat_emitter =
    (Shader.composite
       (Shader.lambert RGBFFF.white)
       (Shader.phong RGBFFF.gray 16.0)),
    Shader.lambert_sample,
    Shader.diffuse_light (RGBFFF.make 4.0 4.0 4.0) in
  let mat_silver =
    Shader.cook_torrance RGBFFF.black 0.1 0.3 61.0 51.0 41.0,
    Shader.cook_torrance_sample RGBFFF.black 0.1,
    Shader.diffuse_light RGBFFF.black in
  let mat_gold =
    Shader.cook_torrance RGBFFF.black 0.1 0.3 33.0 12.0 4.1,
    Shader.cook_torrance_sample RGBFFF.black 0.1,
    Shader.diffuse_light RGBFFF.black in
  let mat_copper =
    Shader.cook_torrance RGBFFF.black 0.1 0.3 31.0 9.0 5.6,
    Shader.cook_torrance_sample RGBFFF.black 0.1,
    Shader.diffuse_light RGBFFF.black in
  let mat_mirror =
    Shader.ideal_reflector RGBFFF.white,
    Shader.ideal_reflector_sample,
    Shader.diffuse_light RGBFFF.black in
  let mat_diamond =
    Shader.ideal_refractor (RGBFFF.make 0.9 0.9 0.9),
    Shader.ideal_refractor_sample 2.417,
    Shader.diffuse_light RGBFFF.black in
  let mat_ruby =
    Shader.ideal_refractor (RGBFFF.make 1.0 0.5 0.5),
    Shader.ideal_refractor_sample 2.417,
    Shader.diffuse_light RGBFFF.black in
  let mat_glass =
    Shader.ideal_refractor RGBFFF.white, (* direct illuminationを加えると真っ白 *)
    Shader.ideal_refractor_sample 4.0,
    Shader.diffuse_light RGBFFF.black in
  (*let robj =
    Union.rt
      [Sphere.rt
         (Sphere.make (Point3.make (-1.0) 0.0 (sqrt 3.0)) 1.0)
         mat_light_red;
       Sphere.rt
         (Sphere.make (Point3.make 1.0 0.0 (sqrt 3.0)) 1.0)
         mat_light_green;
       Sphere.rt
         (Sphere.make (Point3.make 0.0 0.0 0.0) 1.0)
         mat_light_blue;
       Sphere.rt
         (Sphere.make (Point3.make 0.0 (sqrt 3.0) (2.0 *. (sqrt 3.0) /. 3.0)) 1.0)
         mat_white;
       Plane.rt
         (Plane.make (Point3.make 0.0 (-1.0) 0.0) (Vector3.make 0.0 1.0 0.0))
         mat_light_gray] in*)
  (*let s = 1.0 /. (sqrt 2.0) in
  let robj =
    Union.rt
      [Difference.rt
         (Sphere.rt
           (Sphere.make (Point3.make 0.0 0.0 0.0) 1.0)
           mat_white)
         (Union.rt
           [Sphere.rt
              (Sphere.make (Point3.make 0.0 0.0 1.0) 0.25)
              mat_white;
            Sphere.rt
              (Sphere.make (Point3.make (-.s) 0.0 s) 0.25)
              mat_white;
            Sphere.rt
              (Sphere.make (Point3.make s 0.0 s) 0.25)
              mat_white;
            Sphere.rt
              (Sphere.make (Point3.make 0.0 (-.s) s) 0.25)
              mat_white;
            Sphere.rt
              (Sphere.make (Point3.make 0.0 s s) 0.25)
              mat_white]);
       Plane.rt
         (Plane.make (Point3.make 0.0 (-1.0) 0.0) (Vector3.make 0.0 1.0 0.0))
         mat_light_gray] in*)
  (*let robj =
    Union.rt
      [Sphere.rt
         (Sphere.make (Point3.make (-1.0) 0.0 0.0) 1.0)
         mat_silver;
       Sphere.rt
         (Sphere.make (Point3.make 0.0 (sqrt 3.0) 0.0) 1.0)
         mat_gold;
       Sphere.rt
         (Sphere.make (Point3.make 1.0 0.0 0.0) 1.0)
         mat_copper;
       Plane.rt
         (Plane.make (Point3.make 0.0 (-1.0) 0.0) (Vector3.make 0.0 1.0 0.0))
         mat_light_gray] in*)
  (*let robj =
    Union.rt
      [Sphere.rt
         (Sphere.make (Point3.make (-0.8) (-1.0) 0.0) 1.0)
         mat_mirror;
       Sphere.rt
         (Sphere.make (Point3.make 0.8 (-1.0) 1.8) 1.0)
         mat_glass;
       Plane.rt
         (Plane.make (Point3.make (-2.0) 0.0 0.0) (Vector3.make 1.0 0.0 0.0))
         mat_red;
       Plane.rt
         (Plane.make (Point3.make 2.0 0.0 0.0) (Vector3.make (-1.0) 0.0 0.0))
         mat_blue;
       Plane.rt
         (Plane.make (Point3.make 0.0 0.0 (-2.0)) (Vector3.make 0.0 0.0 1.0))
         mat_white;
       Plane.rt
         (Plane.make (Point3.make 0.0 0.0 2.0) (Vector3.make 0.0 0.0 (-1.0)))
         mat_white;
       Plane.rt
         (Plane.make (Point3.make 0.0 (-2.0) 0.0) (Vector3.make 0.0 1.0 0.0))
         mat_white;
       Plane.rt
         (Plane.make (Point3.make 0.0 2.0 0.0) (Vector3.make 0.0 (-1.0) 0.0))
         mat_emitter] in*)
  let robj =
    Union.rt
      [Mesh.rt
         (Mesh.load "./data/diamond.obj" 2.0 false)
         mat_ruby;
       Plane.rt
         (Plane.make (Point3.make 0.0 (-2.0) 0.0) (Vector3.make 0.0 1.0 0.0))
         mat_light_gray] in
(*  let lights =
    [Light.parallel (RGBFFF.make 1.0 0.5 0.5) (Vector3.make 1.0 (-1.0) (-1.0));
     Light.parallel (RGBFFF.make 0.5 0.5 1.0) (Vector3.make (-1.0) (-1.0) (-1.0))] in*)
(*  let lights =
    [Light.parallel RGBFFF.white (Vector3.make 0.0 (-1.0) (-1.0))] in*)
  let lights = [] in
  let env n =
    if n = 1 then
      RGBFFF.make 0.0 0.0 0.5 (* background *)
    else
      RGBFFF.make 2.0 2.0 2.0 (* sky color *) in
  (*let env n = RGBFFF.black in*)
  let camera = Camera.move_to (Point3.make 0.0 0.0 6.0) (Camera.make Frame3.cartesian 1.0 1.0 1.0) in
  let img = Image.make 256 256 in
  let graphics =
    try
      Graphics.open_graph (Printf.sprintf " %dx%d+%d-%d" img.Image.width img.Image.height 120 240);
      true
    with Graphics.Graphic_failure(msg) ->
      print_string (msg^"\n");
      flush stdout;
      false in
  let timer = Timer.make () in
  render robj lights env camera img graphics;
  let tm = timer () in
  Printf.printf "rendering took %f sec.\n" tm;
  Image.save img "scene001.bmp";
  print_string "press enter key:";
  flush stdout;
  input_line stdin
