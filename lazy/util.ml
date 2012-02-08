open ExtList

(** Utility functions *)

(** {6 Exceptions} *)

exception ToBeImplemented

(** {6 Functions on options} *)

let opt2list = function None -> [] | Some(x) -> [x]
let rec pr_opt epr none ppf x =
  match x with
    None ->
      Format.fprintf ppf "%s" none
  | Some(x) ->
      Format.fprintf ppf "%a" epr x

(** {6 Functions on tuples} *)

let fst3 (x, _, _) = x
let snd3 (_, x, _) = x
let trd3 (_, _, x) = x

(** {6 Functions on lists} *)

let rec init xs =
 match xs with
   [] -> assert false
 | [x] -> []
 | x::xs' -> x::(init xs')

let filteri p xs =
  let rec aux i xs =
		  match xs with
		    [] -> []
		  | x::xs' ->
		      if p i x then
          x::(aux (i + 1) xs')
        else
		        aux (i + 1) xs'
  in
  aux 0 xs

let rec map3 f xs ys zs =
  match xs, ys, zs with
    [], [], [] ->
      []
  | x::xs', y::ys', z::zs' ->
      f x y z::map3 f xs' ys' zs'
  | _ -> assert false

let rec zip3 xs ys zs =
  match xs, ys, zs with
    [], [], [] ->
      []
  | x::xs', y::ys', z::zs' ->
      (x, y, z)::zip3 xs' ys' zs'
  | _ -> assert false

let rec unzip3 ls =
  match ls with
    [] ->
      [], [], []
  | (x, y, z)::ls ->
      let (xs, ys, zs) = unzip3 ls in
      x::xs, y::ys, z::zs

let rec iter3 f xs ys zs =
  match xs, ys, zs with
    [], [], [] ->
      ()
  | x::xs', y::ys', z::zs' ->
      let () = f x y z in
      iter3 f xs' ys' zs'
  | _ -> assert false

let rec find_map f xs =
  match xs with
    [] -> raise Not_found
  | x::xs' ->
      try
        f x
      with Not_found ->
        find_map f xs'

let rec concat_map f xs =
  match xs with
    [] ->
      []
  | x::xs' ->
      let x = f x in
      let xs' = concat_map f xs' in
      x @ xs'

let partition_map p ys =
  let rec aux ls rs xs =
    match xs with
      [] -> ls, rs
    | x::xs' ->
        (match p x with
          `L(y) -> aux (y::ls) rs xs'
        | `R(y) -> aux ls (y::rs) xs'
        | _ -> failwith "Util.partition_map")
  in
  aux [] [] (List.rev ys)

let map_left f xs =
  let rec aux ys xs =
    match xs with
      [] -> ys
    | x::xs ->
        let y = f ys x xs in
        aux (ys @ [y]) xs
  in
  aux [] xs

let map_right f xs =
  let rec aux xs ys =
    match xs with
      [] -> ys
    | x::xs ->
        let y = f (List.rev xs) x ys in
				    aux xs (y::ys)
  in
  aux (List.rev xs) []

let map_fold_left f z xs =
  let rec aux ys z xs =
    match xs with
      [] -> ys, z
    | x::xs ->
        let y, z = f ys z x xs in
				    aux (ys @ [y]) z xs
  in
  aux [] z xs

let map_fold_right f xs z =
  let rec aux xs z ys =
    match xs with
      [] -> z, ys
    | x::xs ->
        let z, y = f (List.rev xs) x z ys in
		      aux xs z (y::ys)
  in
  aux (List.rev xs) z []

let rec ctx_of xs i =
  match xs with
    [] -> assert false
  | x::xs' ->
      if i = 0 then
        fun ys -> ys @ xs'
      else if i > 0 then
        let ctx = ctx_of xs' (i - 1) in
        fun ys -> x::ctx ys
      else
        assert false

let all_ctx_elem_of xs =
  List.mapi (fun i x -> ctx_of xs i, x) xs

let rec is_prefix xs ys =
  match xs, ys with
    [], _ -> true
  | x::xs', y::ys' -> x = y && is_prefix xs' ys'
  | _, _ -> false

(** nonemp_prefixes \[1;2;3\] = \[\[1\]; \[1; 2\]; \[1; 2; 3\]\] *)
let nonemp_prefixes ts =
		let _, tss = List.fold_left
		  (fun (ts, tss) t ->
		    ts @ [t], tss @ [ts @ [t]])
		  ([], [])
		  ts
  in
  tss

(** @deprecated use ExtList.List.last *)
let rec last xs =
 match xs with
   [] -> assert false
 | [x] -> x
 | x::xs' -> last xs'

(** @deprecated use ExtList.List.filter_map *)
let rec filter_map p xs =
  match xs with
    [] -> []
  | x::xs' ->
      (match p x with
        Some(y) -> y::(filter_map p xs')
      | None -> filter_map p xs')

(** @deprecated use ExtList.List.split_nth *)
let rec split_nth n xs =
  if n = 0 then
    [], xs
  else if n > 0 then
    (match xs with
      x::xs' ->
        let xs1, xs2 = split_nth (n - 1) xs' in
        x::xs1, xs2
    | _ -> assert false)
  else
    assert false

let rec split_multiple ls xs =
  match ls with
    [] -> [xs]
  | l::ls ->
      let xs1, xs2 = List.split_nth l xs in
      xs1::split_multiple ls xs2

let rec find_split p xs =
  match xs with
    [] -> raise Not_found
  | x::xs' ->
      if p x then
        [], x, xs'
      else
        let ls, y, rs = find_split p xs' in
        x::ls, y, rs

let rec classify eqrel xs =
  match xs with
    [] -> []
  | x::xs' ->
      let t, f = List.partition (fun x' -> eqrel x x') xs' in
      (x::t)::(classify eqrel f)

let multiply_list f xs ys =
  concat_map
    (fun x ->
      List.map (fun y -> f x y) ys)
    xs

let multiply_list_list f xss =
  List.fold_left
    (multiply_list f)
    (List.hd xss)
    (List.tl xss)

let rec pr_list epr sep ppf xs =
  match xs with
    [] ->
      ()
  | [x] ->
      Format.fprintf ppf "%a" epr x
  | x::xs' ->
      Format.fprintf
        ppf
        "%a%a%a"
        epr x
        (fun ppf sep -> Format.fprintf ppf sep) sep
        (pr_list epr sep) xs'

(** {6 Functions on sets} *)

let rec diff xs ys =
  match xs, ys with
    [], [] | [],  _ |  _, [] -> xs
  | x'::xs', ys ->
      if List.mem x' ys then
        diff xs' ys
      else
        x'::(diff xs' ys)

let subset l1 l2 = List.for_all (fun x -> List.mem x l2) l1

(** {6 Functions on matrices} *)

let rec transpose xss =
  if List.for_all (fun xs -> xs = []) xss then
    []
  else
    let xs, xss =
      List.split
        (List.map (function x::xs -> x, xs | _ -> assert false ) xss)
    in
    xs::transpose xss

(** {6 Functions on graphs} *)

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


(** {6 Functions on permutations} *)

let permutations n =
  let rec aux xs =
    match xs with
      [] -> []
    | _ ->
        List.concat
          (List.init
            (List.length xs)
            (fun i ->
              let xs1, x::xs2 = List.split_nth i xs in
              List.map (fun p -> x::p) (aux (xs1 @ xs2))))
  in
  aux (List.init n (fun i -> i))

let maps n1 n2 =
  let xs = List.init n1 (fun i -> i) in
  let yss = permutations n2 in
  List.map (fun ys -> List.combine xs ys) yss

(** {6 Other functions} *)

let rec fixed_point f eq x =
  let x' = f x in
  if eq x x' then x else fixed_point f eq x'

