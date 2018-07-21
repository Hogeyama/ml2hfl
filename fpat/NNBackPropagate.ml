open Util
open Combinator

(** Neural network with learning by backward error propagation *)

type neural_net =
    { unit : int list;
      threshold : float list;
      link : (int * int) list;
      weight : float list;
      layer_in : int list;
      layer_hidden : int list;
      layer_out : int list }

let sigmoid x =
  let beta = 4. in
  1. /. (1. +. exp (-.beta *. x))

let get_link_to nn i = List.filter (fun x -> snd x = i) nn.link
let get_link_from nn i = List.filter (fun x -> fst x = i) nn.link
let get_unit_to nn i = List.map fst (get_link_to nn i)
let get_unit_from nn i = List.map snd (get_link_from nn i)
let is_in_unit nn i = List.mem i nn.layer_in
let is_hidden_unit nn i = List.mem i nn.layer_hidden
let is_out_unit nn i = List.mem i nn.layer_out
let get_input nn i xs = snd (List.find (fun x -> fst x = i) (List.zip nn.layer_in xs))
let get_output nn i ys = snd (List.find (fun x -> fst x = i) (List.zip nn.layer_out ys))
let get_weight nn (i, j) = snd (List.find (fun x -> fst x = (i, j)) (List.zip nn.link nn.weight))
let get_threshold nn i = snd (List.find (fun x -> fst x = i) (List.zip nn.unit nn.threshold))

let rec calc_output nn i xs =
  if is_in_unit nn i then
    get_input nn i xs
  else
    let ws = List.map (get_weight nn) (get_link_to nn i) in
    let os = List.map (fun j -> calc_output nn j xs) (get_unit_to nn i) in
    sigmoid ((Vector.dot ws os) +. (get_threshold nn i))

let rec back_propagate nn (xs, ys) =
  let rec delta i =
    let beta = 4. in
    let o = (calc_output nn i xs) in
    let fp = beta *. o *. (1. -. o) in
    if is_out_unit nn i then
      fp *. ((get_output nn i ys) -. o)
    else if is_hidden_unit nn i then
      let ws = List.map (get_weight nn) (get_link_from nn i) in
      let ds = List.map delta (get_unit_from nn i) in
      fp *. (Vector.dot ws ds)
    else
      assert false
  in
  let dw (j, i) =
    let eta = 1. in
    eta *. delta i *. (calc_output nn j xs) in
  let dth i =
    let eta = 1. in
    if is_in_unit nn i then 0. else eta *. delta i in
  let new_weight = List.map2 (fun x -> fun y -> x +. y) nn.weight (List.map dw nn.link) in
  let new_threshold = List.map2 (fun x -> fun y -> x +. y) nn.threshold (List.map dth nn.unit) in
  { nn with weight = new_weight; threshold = new_threshold }

let result nn = List.map (calc_output nn 3) [[0.; 0.]; [0.; 1.]; [1.; 0.]; [1.; 1.]]
let result_bin nn = List.map (fun x -> if x > 0.5 then 1. else 0.) (result nn)

let train nn =
  let rec train_sub nn n =
    if n < 2000 then
      let n1 = back_propagate nn ([0.;0.], [0.]) in
      let n2 = back_propagate n1 ([0.;1.], [1.]) in
      let n3 = back_propagate n2 ([1.;0.], [1.]) in
      let n4 = back_propagate n3 ([1.;1.], [0.]) in
      if (result_bin n4) = [0.; 1.; 1.; 0.] then (n, n4) else (train_sub n4 (n + 1))
    else
      (-1, nn) in
  train_sub nn 1

(* solve XOR problem *)
let test () =
  Format.printf "@[";
  let sample n =
    { unit = [1;2;3;] @ List.from_to 4 (3 + n);
      threshold = 0. :: 0. :: List.map (Float.rand (-1., 1.)) (List.from_to 1 (n + 1));
      link = List.map (fun x -> (1, x)) (List.from_to 4 (3 + n))
             @ List.map (fun x -> (2, x)) (List.from_to 4 (3 + n))
             @ List.map (fun x -> (x, 3)) (List.from_to 4 (3 + n));
      weight = List.map (Float.rand (-1., 1.)) (List.from_to 1 (3 * n));
      layer_in = [1; 2];
      layer_hidden = List.from_to 4 (3 + n);
      layer_out = [3] }
  in
  let size = (*if Array.length Sys.argv >= 2 then int_of_string Sys.argv.(1) else*) 8 in
  let res = train (sample size) in
  Format.printf "successfully solved!!@,";
  Format.printf "trained %d times@," (fst res);
  for i = 0 to 3 do
    Format.printf
      "%s: %f\n"
      (List.nth ["00"; "01"; "10"; "11"] i)
      (List.nth (result (snd res)) i)
  done;
  Format.printf "@]"
