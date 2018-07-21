open Util
open Combinator

(** Meshes *)

class type ['a] raytraceable =
  object
    (* must be sorted in increasing order *)
    (* Œð“_‚ªk‘Þ‚µ‚½‚ç‚»‚ÌŒð“_‚Í‰Á‚¦‚È‚¢ *)
    method raytrace: Ray.t -> 'a RTPoint3.t list
  end

type t = { vertices: Point3.t array;
           normals: Vector3.t array;
           faces: int array array;
           bounding_sphere: unit raytraceable }

let make vertices faces radius =
  let normals =
    Array.init
      (Array.length faces)
      (fun n ->
        Vector3.normalize
          (Vector3.cross
            (Point3.from_to vertices.(faces.(n).(0)) vertices.(faces.(n).(1)))
            (Point3.from_to vertices.(faces.(n).(0)) vertices.(faces.(n).(2))))) in
  { vertices = vertices;
    normals = normals;
    faces = faces;
    bounding_sphere = Sphere.rt (Sphere.make Point3.o radius) () }

let load filename scale reverse =
  let ic = open_in filename in
  let radius = ref 0.0 in
  let vertices = ref [] in
  let faces = ref [] in
 (try
    while true do
      let line = input_line ic in
      try
        let head = String.sub line 0 2 in
        if head = "v " then
            let x, y, z = Scanf.sscanf line "v %f %f %f" (fun x y z -> scale *. x, scale *. y, scale *. z) in
            vertices := !vertices @ [Point3.make x y z];
            (if !radius < abs_float x then radius := abs_float x);
            (if !radius < abs_float y then radius := abs_float y);
            (if !radius < abs_float z then radius := abs_float z)
        else if head = "f " then
            let v1, v2, v3 = Scanf.sscanf line "f %d %d %d" (fun v1 v2 v3 -> v1, v2, v3) in
            let face = Array.make 3 0 in
            face.(0) <- v1 - 1;
            if reverse then
             (face.(1) <- v3 - 1;
              face.(2) <- v2 - 1)
            else
             (face.(1) <- v2 - 1;
              face.(2) <- v3 - 1);
            faces := !faces @ [face]
        else
          ()(*failwith ""*)
      with _ -> ()
    done
  with End_of_file ->
    close_in ic);
  Printf.printf "loaded file:%s vertices:%d faces:%d radius:%f\n" filename (List.length !vertices) (List.length !faces) !radius;
  make (Array.of_list !vertices) (Array.of_list !faces) !radius

let intersect_triangle_ray p1 p2 p3 ray =
  let edge1 = Point3.from_to p1 p2 in
  let edge2 = Point3.from_to p1 p3 in
  let pvec = Vector3.cross ray.Ray.direction edge2 in
  let det = Vector3.dot pvec edge1 in
  if det = 0.0 then
    None
  else
    let inv_det = 1.0 /. det in
    let tvec = Point3.from_to p1 ray.Ray.origin in
    let barycentric_u = (Vector3.dot tvec pvec) *. inv_det in
    if barycentric_u < 0.0 || barycentric_u > 1.0 then
      None
    else
      let qvec = Vector3.cross tvec edge1 in
      let barycentric_v = (Vector3.dot ray.Ray.direction qvec) *. inv_det in
      if barycentric_v < 0.0 || barycentric_u +. barycentric_v > 1.0 then
        None
      else
        let distance = (Vector3.dot edge2 qvec) *. inv_det in
        if distance < 0.0 then
          None
        else
          Some(distance, det >= 0.0)

let rt mesh attribute =
  object
    method raytrace ray =
      if mesh.bounding_sphere#raytrace ray = [] then
        (* the bounding sphere and the ray have no intersection, and neither do the mesh *)
        []
      else
        List.fast_sort
          RTPoint3.compare
          (List.fold_left
            (fun res n ->
              match intersect_triangle_ray
                      mesh.vertices.(mesh.faces.(n).(0))
                      mesh.vertices.(mesh.faces.(n).(1))
                      mesh.vertices.(mesh.faces.(n).(2))
                      ray with
                None -> res
              | Some(distance, flag) ->
                  let p = RTPoint3.make distance (Ray.point_at ray distance) mesh.normals.(n) flag attribute in
                  p::res)
            []
            (List.from_to 0 (Array.length mesh.faces - 1)))
  end
