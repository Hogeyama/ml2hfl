exception Fatal of string
exception Unsupported of string

let fatal s = raise (Fatal s)
let unsupported s = raise (Unsupported s)

let (|>) x f = f x
let (@@) f x = f x
let do_and_return f x = f x; x
let (|@>) x f = do_and_return f x
let (|*>) x f = x
let (|*@>) x f = x
let (|@*>) x f = x
let assert_ f x = assert (f x)
let assert_false _ = fatal "assert_false"
let (|@) x b f = if b then f x; x
let (|&) x b f = if b then f x else x
let (&>) f x = f x
(* usage: x |@flag&> print *)
(* usage: x |&flag&> trans *)

let flip f x y = f y x
let curry f x y = f (x,y)
let uncurry f (x,y) = f x y

let (@@@) = List.rev_append

module List = struct
  include ExtList.List

  (*** returns a list of integers [m;...;n-1] ***)
  let rec fromto m n =
    if m >= n then
      []
    else
      m :: fromto (m+1) n

  let rev_map_flatten f xs = List.fold_left (fun acc x -> f x @@@ acc) [] xs
  let rev_flatten_map = rev_map_flatten
  let flatten_map f xs = List.rev @@ rev_map_flatten f xs
  let rev_flatten xs = rev_map_flatten Std.identity xs
end

module Array = ExtArray.Array
module Hashtbl = ExtHashtbl.Hashtbl
module String = ExtString.String


let table_create n = Hashtbl.create n;;
let table_find tab x = Hashtbl.find tab x
let table_add tab a b = Hashtbl.add tab a b
(***
  let debug s = (print_string s; print_string "\n"; flush(stdout))
***)
let debugging = ref false;;
let show_time() =
  if
    !debugging
  then
    print_string (string_of_float (Sys.time()))
  else
    ()
let debug s =
  if
    !debugging
  then
    (print_string s; flush stdout)
  else
    ()





let rec uniq_aux ?(cmp=compare) acc = function
    [] -> acc
  | x1::x2::xs when cmp x1 x2 = 0 -> uniq_aux ~cmp acc (x2::xs)
  | x::xs -> uniq_aux ~cmp (x::acc) xs
let uniq ?(cmp=compare) xs = uniq_aux ~cmp [] (List.sort ~cmp xs)
let uniq_sorted ?(cmp=compare) xs = uniq_aux ~cmp [] xs



let diff ?(cmp=compare)  l1 l2 = List.filter (fun x -> List.for_all (fun y -> cmp x y <> 0) l2) l1
let inter ?(cmp=compare) l1 l2 = List.filter (fun x -> List.exists (fun y -> cmp x y = 0) l2) l1
let subset l1 l2 = List.for_all (fun x -> List.mem x l2) l1
let set_eq l1 l2 = subset l1 l2 && subset l2 l1
let union ?(cmp=compare) l1 l2 = List.fold_left (fun l x -> if List.exists (fun y -> cmp x y = 0) l then l else x::l) l2 l1



let rec tabulate n f acc =
  if n < 0 then raise (Invalid_argument "tabulate")
  else
    match n with
    | 0 -> List.rev acc
    | _ -> tabulate (n-1) f (f n::acc)
let tabulate n f = tabulate n f []


let rec fold_left2_neq f acc xs ys =
  match xs, ys with
  | x::xs, y::ys -> fold_left2_neq f (f acc x y) xs ys
  | _ -> acc










let assoc_exn k kts t =
  try
    List.assoc k kts
  with Not_found -> t



let uniq_flatten_map cmp f xs = uniq ~cmp (List.rev_map_flatten f xs)


let is_uppercase c = 'A' <= c && c <= 'Z'


let get_time () =
  let times = Unix.times() in
  times.Unix.tms_utime +. times.Unix.tms_cutime

let add_time tmp t = t := !t +. get_time () -. tmp


let print_err s =
  Format.fprintf Format.err_formatter "%s" s
let print_time () =
  Format.fprintf Format.err_formatter "%f\n" (get_time ())



let rec decomp_snoc = function
  | [] -> failwith "dcomp_snoc"
  | [x] -> [], x
  | x::xs ->
      let xs',y = decomp_snoc xs in
      x::xs', y



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


let rec print_list_aux print punc last fm xs =
  match xs with
      [] -> ()
    | [x] when last ->
        Format.fprintf fm "%a" print x;
        Format.fprintf fm punc
    | [x] -> print fm x
    | x::xs ->
        Format.fprintf fm "%a" print x;
        Format.fprintf fm punc;
        Format.fprintf fm "@,%a" (print_list_aux print punc last) xs
let print_list print ?(first=false) ?(last=false) punc fm xs =
  let punc' = format_of_string punc in
    Format.fprintf fm "@[";
    if first then Format.fprintf fm punc';
    Format.fprintf fm "%a" (print_list_aux print punc' last) xs;
    Format.fprintf fm "@]"


let is_prefix_string pre s =
  let n = String.length pre in
    String.length s >= n && String.sub s 0 n = pre

let split_string s n =
  String.sub s 0 n, String.sub s n (String.length s - n)

let count_line s =
  let n = ref 0 in
    String.iter (fun c -> if c = '\n' then incr n) s;
    !n

let count_list f xs =
  List.fold_left (fun acc n -> if f n then acc+1 else acc) 0 xs

let make_string_of pp =
  fun x ->
    pp Format.str_formatter x;
    Format.flush_str_formatter ()


let print_begin_end ?(str1="BEGIN\n") ?(str2="END\n") f =
  Format.printf "%s@?" str1;
  let r = f () in
  Format.printf "%s@?" str2;
  r

let exists_loop tbl =
  let checked = Hashtbl.create (Hashtbl.length tbl) in
  let f x xs =
    if Hashtbl.mem checked x
    then ()
    else
      let rec aux visited y =
        if List.mem y visited
        then raise Exit
        else
          let visited' = y::visited in
          let ys = try Hashtbl.find tbl y with Not_found -> [] in
          if ys = []
          then visited'
          else List.flatten @@ List.map (aux visited') ys
      in
      let loop_free = aux [] x in
      List.iter (fun y -> Hashtbl.add checked y ()) loop_free
  in
  try
    Hashtbl.iter f tbl;
    false
  with Exit -> true


let rec insert compare x xs =
  match xs with
  | [] -> [x]
  | x'::xs' when compare x x' > 0 -> x'::insert compare x xs'
  | _ -> x::xs

let rec insert_sort compare xs =
  match xs with
  | [] -> []
  | x::xs' -> insert compare x @@ insert_sort compare xs'


let rec repeat f n s =
  if n <= 0
  then s
  else repeat f (n-1) (f s)
