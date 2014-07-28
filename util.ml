exception Fatal of string
exception Unsupported of string

let fatal s = raise (Fatal s)
let unsupported s = raise (Unsupported s)
let invalid_argument s = raise (Invalid_argument s)

let (-|) f g x = f (g x)
let (|-) f g x = g (f x)
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

let (=>) b1 b2 = not b1 || b2

let flip f x y = f y x
let curry f x y = f (x,y)
let uncurry f (x,y) = f x y
let fst3 (x,y,z) = x
let snd3 (x,y,z) = y
let trd (x,y,z) = z

let (@@@) = List.rev_append

let rec print_list_aux print punc last fm xs =
  match xs with
  | [] -> ()
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
  if last then Format.fprintf fm punc';
  Format.fprintf fm "@]"

module IntSet =
  Set.Make(
    struct
      type t = int
      let compare = compare
    end)

module StringSet = Set.Make(String)

module List = struct
  include ExtList.List

  let singleton x = [x]
  let cons x xs = x::xs
  let snoc xs x = xs@[x]

  let print pr fm xs = Format.printf "[%a]" (print_list pr "; ") xs

  (*** returns a list of integers [m;...;n-1] ***)
  let rec fromto m n =
    if m >= n then
      []
    else
      m :: fromto (m+1) n

  let rev_map_flatten f xs = fold_left (fun acc x -> f x @@@ acc) [] xs
  let rev_flatten_map = rev_map_flatten
  let flatten_map f xs = rev @@ rev_map_flatten f xs
  let rev_flatten xs = rev_map_flatten Std.identity xs

  let rec tabulate n f acc =
    if n < 0 then raise (Invalid_argument "tabulate")
    else
      match n with
      | 0 -> rev acc
      | _ -> tabulate (n-1) f (f n::acc)
  let tabulate n f = tabulate n f []

  let assoc_default k tbl x =
    try
      assoc k tbl
    with Not_found -> x

  let count_line s =
    let n = ref 0 in
    String.iter (fun c -> if c = '\n' then incr n) s;
    !n

  let count_list f xs =
    fold_left (fun acc n -> if f n then acc+1 else acc) 0 xs

  let rec decomp_snoc = function
    | [] -> failwith "decomp_snoc"
    | [x] -> [], x
    | x::xs ->
        let xs',y = decomp_snoc xs in
        x::xs', y

  let rec mapi2 f i xs ys =
    match xs,ys with
    | [],[] -> []
    | x::xs',y::ys' -> f i x y :: mapi2 f (i+1) xs' ys'
    | _ -> raise (Invalid_argument "List.mapi2")
  let mapi2 f xs ys = mapi2 f 0 xs ys

  let rec rev_map2 f acc xs ys =
    match xs,ys with
    | [],[] -> acc
    | x::xs',y::ys' -> rev_map2 f (f x y::acc) xs' ys'
    | _ -> raise (Invalid_argument "List.rev_map2")
  let rev_map2 f xs ys = rev_map2 f [] xs ys

  let rec map3 f xs ys zs =
    match xs,ys,zs with
    | [],[],[] -> []
    | x::xs',y::ys',z::zs' -> f x y z :: map3 f xs' ys' zs'
    | _ -> raise (Invalid_argument "List.map3")

  let rec filter_out f xs = filter (not -| f) xs

  let rec rev_split acc1 acc2 xs =
    match xs with
    | [] -> acc1, acc2
    | (x,y)::xs' -> rev_split (x::acc1) (y::acc2) xs'
  let rev_split xs = rev_split [] [] xs
  let split_map f = rev_split -| rev_map f

  let replace_nth xs i y = List.mapi (fun j x -> if j = i then y else x) xs

  let diff ?(cmp=compare)  l1 l2 = filter (fun x -> for_all (fun y -> cmp x y <> 0) l2) l1
  let inter ?(cmp=compare) l1 l2 = filter (fun x -> exists (fun y -> cmp x y = 0) l2) l1
  let subset l1 l2 = for_all (fun x -> mem x l2) l1
  let set_eq l1 l2 = subset l1 l2 && subset l2 l1
  let union ?(cmp=compare) l1 l2 = fold_left (fun l x -> if exists (fun y -> cmp x y = 0) l then l else x::l) l2 l1
end

module Array = ExtArray.Array

module Hashtbl = ExtHashtbl.Hashtbl

module String = struct
  include ExtString.String

  let split_nth s n =
    sub s 0 n, sub s n (length s - n)

  let rsplit s sep =
    let subs = nsplit s sep in
    match subs with
    | [] -> assert false
    | [s] -> raise ExtString.Invalid_string
    | _ ->
        let subs',s2 = List.decomp_snoc subs in
        let s1 = List.fold_left (fun s1 s2 -> s1 ^ sep ^ s2) (List.hd subs') (List.tl subs') in
        s1, s2
end

module Option = struct
  include Option

  let iter = may
  let apply = map
end

module Pair = struct
  let swap (x,y) = y, x
  let make f g x = f x, g x
  let map f g (x,y) = f x, g y
  let map_fst f (x,y) = f x, y
  let map_snd f (x,y) = x, f y
end

let is_uppercase c = 'A' <= c && c <= 'Z'


let get_time () =
  let times = Unix.times() in
  times.Unix.tms_utime +. times.Unix.tms_cutime

let add_time tmp t = t := !t +. get_time () -. tmp


let print_err s =
  Format.fprintf Format.err_formatter "%s" s
let print_time () =
  Format.fprintf Format.err_formatter "%f\n" (get_time ())



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


let rec my_input ic s ofs len acc =
  if len = 0
  then acc
  else
    let r = input ic s ofs len in
    if r > 0
    then my_input ic s (ofs+r) (len-r) (acc+r)
    else acc
let my_input ic s ofs len = my_input ic s ofs len 0


let make_string_of pp =
  fun x ->
    pp Format.str_formatter x;
    Format.flush_str_formatter ()


let print_begin_end ?(str1="BEGIN\n") ?(str2="END\n") f =
  Format.printf "%s@?" str1;
  let r = f () in
  Format.printf "%s@?" str2;
  r


let rec repeat f n x =
  if n <= 0
  then x
  else repeat f (n-1) (f x)


(* TODO: support escaping *)
let split_spaces s =
  let spaces = [' '; '\t'] in
  let quotations = ['"'; '\''] in
  let rec chop_spaces s =
    if s <> "" && List.mem s.[0] spaces
    then chop_spaces @@ String.lchop s
    else s
  in
  let rec take quot acc_rev s =
    if s = ""
    then
      begin
        if Option.is_some quot then invalid_argument "split_spaces";
        String.implode @@ List.rev acc_rev, ""
      end
    else
      if quot = None && List.mem s.[0] (spaces@quotations)
         || Option.is_some quot && s.[0] = Option.get quot
      then
        String.implode @@ List.rev acc_rev, String.lchop s
      else
        take quot (s.[0]::acc_rev) @@ String.lchop s
  in
  let rec aux acc_rev s =
    if s = ""
    then List.rev acc_rev
    else
      let s' = chop_spaces s in
      let s'',quot =
        if List.mem s'.[0] quotations
        then String.lchop s', Some s'.[0]
        else s', None
      in
      let s''',s_rest = take quot [] s'' in
      let acc_rev' = if s''' = "" then acc_rev else s'''::acc_rev in
      aux acc_rev' s_rest
  in
  aux [] s
