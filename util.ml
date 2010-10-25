

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
  if n = 0
  then m
  else
    if m <= n then gcd m (n mod m)
    else gcd n (m mod n)

