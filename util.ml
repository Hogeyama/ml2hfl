

let rec uniq_aux acc = function
    [] -> acc
  | x1::x2::xs when x1=x2 -> uniq_aux acc (x2::xs)
  | x::xs -> uniq_aux (x::acc) xs
let uniq xs = uniq_aux [] (List.sort compare xs)
let uniq_sorted xs = uniq_aux [] xs



let (@@) = List.rev_append
let (@@@) xs ys = uniq_sorted (List.merge compare xs ys)


let diff l1 l2 = List.filter (fun x -> not(List.mem x l2)) l1
let inter l1 l2 = List.filter (fun x -> List.mem x l2) l1
let subset l1 l2 = List.for_all (fun x -> List.mem x l2) l1
let set_eq l1 l2 = subset l1 l2 && subset l2 l1



let rec take xs n =
  match xs,n with
      _,0 -> []
    | [],_ -> []
    | x::xs',_ -> x::(take xs' (n-1))

let rec take2 xs n acc =
  match xs,n with
      _,0
    | [],_ -> List.rev acc, xs
    | x::xs',_ -> take2 xs' (n-1) (x::acc)
let take2 xs n = take2 xs n []




let rec tabulate n f acc =
  if n < 0 then assert false
  else
  match n with
      0 -> List.rev acc
    | _ -> tabulate (n-1) f (f n::acc)
let tabulate n f = tabulate n f []


let rec mapi f n = function
  [] -> []
  | x::xs -> f n x :: mapi f (n+1) xs
let mapi f = mapi f 0




let rec fold_left2_neq f acc xs ys =
  match xs, ys with
      x::xs, y::ys -> fold_left2_neq f (f acc x y) xs ys
    | _ -> acc




let rev_map_flatten f xs = List.fold_left (fun acc x -> f x @@ acc) [] xs
let rev_flatten_map = rev_map_flatten



let rev_flatten xs = rev_map_flatten (fun x -> x) xs






let assoc_exn k kts t =
  try
    List.assoc k kts
  with Not_found -> t





let uniq_flatten_map f xs = uniq (rev_map_flatten f xs)



let rec last = function
    [] -> assert false
  | [x] -> x
  | x::xs -> last xs




let is_uppercase c =
  'A' <= c && c <= 'Z'





let print_msg s = Format.printf "%s@." s



let get_time () = (Unix.times()).Unix.tms_utime +. (Unix.times()).Unix.tms_cutime


let add_time tmp t = t := !t +. get_time () -. tmp




let print_err s =
  Format.fprintf Format.err_formatter "%s" s
let print_time () =
  Format.fprintf Format.err_formatter "%f\n" (get_time ())





let rec last = function
    [] -> failwith "last"
  | [x] -> x
  | x::xs -> last xs



let rec gcd m n =
  if m < n then gcd n m
  else if n = 0 then m
  else gcd n (m mod n)


(* graph *)
let save_as_dot filename vertices edges =
  let oc = open_out filename in 
  let ocf =
    Format.make_formatter
      (output oc)
      (fun () -> flush oc) in
  Format.fprintf ocf "@[<v>digraph flow {@ ";
  List.iter
    (fun (vertex, attribute) ->
      Format.fprintf ocf "  \"%s\" %s@ " vertex attribute)
    vertices;
  List.iter
    (fun (vertex1, vertex2, attribute) ->
      Format.fprintf ocf "  \"%s\" -> \"%s\" %s@ " vertex1 vertex2 attribute)
    edges;
  Format.fprintf ocf "}@]@?";
  close_out oc

let rec classify eqrel xs =
 match xs with
   [] -> []
 | x::xs' ->
     let t, f = List.partition (fun x' -> eqrel x x') xs' in
     (x::t)::(classify eqrel f)

let rec filterwo p xs =
  let rec aux xs1 xs2 =
    match xs2 with
      [] -> []
    | x::xs -> if p x (xs1 @ xs) then x::(aux (x::xs1) xs) else aux (x::xs1) xs
  in aux [] xs

let is_int s = try ignore (int_of_string s); true with Failure "int_of_string" -> false

let rec my_input ic s ofs len acc =
  if len = 0
  then acc
  else
    let r = input ic s ofs len in
      if r > 0
      then my_input ic s (ofs+r) (len-r) (acc+r)
      else acc
let my_input ic s ofs len = my_input ic s ofs len 0

let apply_opt f = function
    None -> None
  | Some x -> Some (f x)
