exception Fatal of string
exception Unsupported of string

let fatal s = raise (Fatal s)
let unsupported s = raise (Unsupported s)

let (|>) x f = f x
let (@@) f x = f x

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


(*** returns a list of integers [m;...;n-1] ***)
let rec fromto m n =
  if m>=n then [] else m::(fromto (m+1) n);;

let rec list_repl n a l =
  match l with
    [] -> raise (Fatal "list_repl: position is wrong")
  | x::l' ->
     if n=0 then a::(List.tl l)
     else x::(list_repl (n-1) a l')

let rec list_take_nth l n =
  match l with
    [] -> raise (Fatal "list_take_nth: position is wrong")
  | a::l' ->
     if n=0 then (a, l')
     else
       let (x, l'') = list_take_nth l' (n-1) in
         (x, a::l'')

let rec merge_and_unify comp l1 l2 =
  match (l1, l2) with
    (_,[]) -> l1
  | ([], _)->l2
  | (x::l1',y::l2') ->
        let c = comp x y in
         if c=0 then x::(merge_and_unify comp l1' l2')
         else if c<0 then x::(merge_and_unify comp l1' l2)
         else y::(merge_and_unify comp l1 l2');;
let rec merge comp l1 l2 =
  match (l1, l2) with
    (_,[]) -> l1
  | ([], _)->l2
  | (x::l1',y::l2') ->
        let c = comp x y in
         if c<0 then x::(merge comp l1' l2)
         else y::(merge comp l1 l2');;
let list_flatten l =  List.fold_left (@) [] l;;

(*** utility functions ***)
let id x = x;;
let rec delete_duplication l =
  match l with
    [] -> []
  | [x] -> [x]
  | x::y::l -> if x=y then delete_duplication (y::l)
               else x::(delete_duplication (y::l));;

let delete_duplication_unsorted c =
  let c' = List.sort compare c in
    delete_duplication c';;

let rec delete_duplication2 comp comb l =
  match l with
    [] -> []
  | [x] -> [x]
  | x::y::l -> if comp x y =0 then delete_duplication2 comp comb ((comb x y)::l)
               else x::(delete_duplication2 comp comb (y::l));;
let rec list_assoc2 x l =
  match l with
    [] -> raise Not_found
  | (y,v)::l' -> if x=y then (v, l')
                 else let (v1,l1) = list_assoc2 x l' in (v1, (y,v)::l1);;
let list_diff l1 l2 =
  List.filter (function x-> not(List.mem x l2)) l1;;
let rec list_last l =
  match l with
     [] -> raise Not_found
  | [x] -> x
  | x::l' -> list_last(l');;

let rec list_last_and_rest l =
  match l with
     [] -> raise Not_found
  | [x] -> (x, [])
  | x::l' ->
     let (y, l'') = list_last_and_rest(l')
     in (y, x::l'')

let rec subset_sortedlist comp l1 l2 =
  match l1 with
    [] -> true
  | x::l1' ->
      match l2 with
         [] -> false
       | y::l2' ->
          let c = comp x y in
          if c=0 then subset_sortedlist comp l1' l2'
          else if c<0 then false
          else subset_sortedlist comp l1 l2'

let swap (x,y) = (y,x);;

(*** substitutions ***)
type ('a, 'b) substitution = ('a * 'b) list
let subst_empty = []
let subst_var s var default =
  try
     List.assoc var s
  with
     Not_found -> default
let make_subst x v = [(x,v)]
let list2subst x = x
let comp_subst subst s1 s2 =
  let s2' = List.map (fun (x,v)->(x,subst s1 v)) s2 in
  let s1' = List.filter (fun (x,v) -> List.for_all (fun (x',v')->x!=x') s2) s1 in
    s1'@s2'

type ('a, 'b) env = ('a * 'b) list
let env_lookup = List.assoc
let env_empty = []
let env_extend x v env = (x,v)::env
let env_map f env = List.map (fun (x,v)->(x,f v)) env
let print_env f g env =
  let rec print_seq env =
    match env with
      [] -> ()
    | (x,v)::env' -> (print_string (f x); print_string " : ";
                      print_string (g v); print_string "\n";
                      print_seq env')
  in
     (print_string "{\n"; print_seq env; print_string "}\n")

let env2list x = x;;
let list2env x = x;;

(*** perfect_matching checks to see if there exists a perfect matching
 *** The implementation is extremely naive, assuming
 *** that the size of the input graph is very small.
 *** For a large graph, an approximate, conservative
 *** algorithm should be used.
 ***)
let rec delete x l =
  match l with
    [] -> raise Not_found
  | y::l' -> if x=y then l' else y::(delete x l')

let rec find nodes candidates =
  match candidates with
    [] -> true
  | nodes1::candidates' ->
      List.exists (fun x->
                    try let nodes' = delete x nodes in find nodes' candidates'
                    with
                      Not_found -> false)
      nodes1

let perfect_matching nodes1 nodes2 edges =
 let get_neighbors x = List.map snd (List.filter (fun (x',_) -> x=x') edges) in
 let sources = List.map fst edges in
 let sinks = List.map snd edges in
 if (List.exists (fun x -> not(List.mem x sources)) nodes1)
    || (List.exists (fun x -> not(List.mem x sinks)) nodes2)
    || List.length nodes1 != List.length nodes2
 then false (*** there is a node that is unrelated ***)
 else
   let neighbors = List.map get_neighbors nodes1 in
     find nodes2 neighbors

(*** hash ***)
let list2hash l =
  let h = Hashtbl.create (2*(List.length l)) in
    (List.iter (fun (x,y) -> Hashtbl.add h x y) l; h)

let hash2list h =
  Hashtbl.fold (fun x -> fun y -> fun l -> (x,y)::l) h []


(******************)


let rec uniq_aux ?(cmp=compare) acc = function
    [] -> acc
  | x1::x2::xs when cmp x1 x2 = 0 -> uniq_aux ~cmp acc (x2::xs)
  | x::xs -> uniq_aux ~cmp (x::acc) xs
let uniq ?(cmp=compare) xs = uniq_aux ~cmp [] (List.sort cmp xs)
let uniq_sorted ?(cmp=compare) xs = uniq_aux ~cmp [] xs

let (@@@) = List.rev_append


let diff l1 l2 = List.filter (fun x -> not(List.mem x l2)) l1
let inter ?(cmp=compare) l1 l2 = List.filter (fun x -> List.exists (fun y -> cmp x y = 0) l2) l1
let subset l1 l2 = List.for_all (fun x -> List.mem x l2) l1
let set_eq l1 l2 = subset l1 l2 && subset l2 l1
let union ?(cmp=compare) l1 l2 = List.fold_left (fun l x -> if List.exists (fun y -> cmp x y = 0) l then l else x::l) l2 l1



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
let mapi f xs = mapi f 0 xs

let rec iteri f n = function
    [] -> ()
  | x::xs -> f n x; iteri f (n+1) xs
let iteri f xs = iteri f 0 xs



let rec fold_left2_neq f acc xs ys =
  match xs, ys with
      x::xs, y::ys -> fold_left2_neq f (f acc x y) xs ys
    | _ -> acc




let rev_map_flatten f xs = List.fold_left (fun acc x -> f x @@@ acc) [] xs
let rev_flatten_map = rev_map_flatten
let flatten_map f xs = List.rev (rev_map_flatten f xs)


let rev_flatten xs = rev_map_flatten (fun x -> x) xs






let assoc_exn k kts t =
  try
    List.assoc k kts
  with Not_found -> t





let uniq_flatten_map cmp f xs = uniq ~cmp (rev_map_flatten f xs)



let is_uppercase c =
  'A' <= c && c <= 'Z'





let get_time () =
  let times = Unix.times() in
  times.Unix.tms_utime +. times.Unix.tms_cutime


let add_time tmp t = t := !t +. get_time () -. tmp




let print_err s =
  Format.fprintf Format.err_formatter "%s" s
let print_time () =
  Format.fprintf Format.err_formatter "%f\n" (get_time ())





let rec decomp_snoc = function
    [] -> failwith "dcomp_snoc"
  | [x] -> [], x
  | x::xs ->
      let xs',y = decomp_snoc xs in
      x::xs', y

let last xs = snd @@ decomp_snoc xs
let init xs = fst @@ decomp_snoc xs





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


let get_opt_val = function
    None -> raise (Failure "get_opt_val")
  | Some t -> t


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


let print_begin_end str1 exp str2 =
  Format.printf "%s@?" str1;
  let r = Lazy.force exp in
  Format.printf "%s@?" str2;
  r

let do_and_return f x = f x; x
