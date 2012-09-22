open ExtList
open ExtString
open Num

(** Utility functions *)

(** {6 Exceptions} *)

exception NotImplemented of string

(** {6 Combinators} *)

(** the compose operator *)
let (-|) f g x = f (g x)
let (-||) f g x y = f (g x y)

(** the reverse compose operator *)
let (|-) f g x = g (f x)
let (||-) f g x y = g (f x y)

(** the pipeline operator *)
let (|>) x f = f x

let rec fixed_point f eq x =
  let x' = f x in
  if eq x x' then x else fixed_point f eq x'

(** {6 Functions on integers} *)

let sum ns = List.fold_left (+) 0 ns

(** @return the greatest common divisor of ns
    @require List.forall (fun n -> n >= 0) ns
    @ensure ret <> 0 *)
let gcd ns =
  let rec aux n1 n2 =
    let _ =
      if (*!Global.debug &&*) not (n1 >= 0 && n2 >= 0) then
        let _ = Format.printf "%d, %d@," n1 n2 in
        assert false
    in
    if n1 < n2 then
      aux n2 n1
    else if n2 = 0 then
      let _ = assert (n1 <> 0) in
      n1
    else
      aux n2 (n1 mod n2)
  in
  match List.filter (fun n -> n <> 0) ns with
    [] ->
      1(*invalid_arg "Util.gcd"*)
  | n :: ns ->
      List.fold_left aux n ns

(** {6 Functions on floating point numbers} *)

let num_of_float f =
  let num_of_positive_float f =
    let m, e = frexp f in
    let sm = string_of_float m in
    let s = String.make 16 '0' in
    (** sm starts with "0." *)
    let _ = String.blit sm 2 s 0 (String.length sm - 2) in
    let e' = power_num (Int 2) (num_of_int e) in
    div_num (mult_num (num_of_string s) e') (power_num (Int 10) (Int 16))
  in
  let n = int_of_float f in
  if f = float_of_int n then
    Int n
  else if f < 0.0 then
    let num = num_of_positive_float (abs_float f) in
    minus_num num
  else num_of_positive_float f

(** {6 Functions on strings} *)

let is_int s =
  try
    let _ = int_of_string s in
    true
  with Failure _ ->
    false

(** {6 Functions on options} *)

let list_of_opt = function None -> [] | Some(x) -> [x]

let rec pr_opt epr none ppf opt =
  match opt with
    None ->
      Format.fprintf ppf "%s" none
  | Some(x) ->
      Format.fprintf ppf "%a" epr x

let fold_opt none some opt =
  match opt with
    None ->
      none
  | Some(x) ->
      some x

(** {6 Functions on tuples} *)

let fst3 (x, _, _) = x
let snd3 (_, x, _) = x
let trd3 (_, _, x) = x

(** {6 Functions on lists} *)

(** {5 Composed operations} *)

let flatten_unzip xs =
  let ls, rs = List.split xs in
  List.flatten ls, List.flatten rs

let rec concat_map f xs =
  match xs with
    [] ->
      []
  | x :: xs' ->
      let r = f x in
      let rs' = concat_map f xs' in
      r @ rs'

let rec concat_map2 f xs ys =
  match xs, ys with
    [], [] ->
      []
  | x :: xs', y :: ys' ->
      let r = f x y in
      let rs' = concat_map2 f xs' ys' in
      r @ rs'

let partition_map p ys =
  let rec aux ls rs xs =
    match xs with
      [] -> ls, rs
    | x :: xs' ->
        (match p x with
          `L(y) -> aux (y :: ls) rs xs'
        | `R(y) -> aux ls (y :: rs) xs'
        | _ -> failwith "Util.partition_map")
  in
  aux [] [] (List.rev ys)

let partition3_map p ys =
  let rec aux ls1 ls2 ls3 xs =
    match xs with
      [] -> ls1, ls2, ls3
    | x :: xs' ->
        (match p x with
          `A(y) -> aux (y :: ls1) ls2 ls3 xs'
        | `B(y) -> aux ls1 (y :: ls2) ls3 xs'
        | `C(y) -> aux ls1 ls2 (y :: ls3) xs'
        | _ -> failwith "Util.partition3_map")
  in
  aux [] [] [] (List.rev ys)

(** @deprecated use ExtList.List.filter_map *)
let rec filter_map p xs =
  match xs with
    [] -> []
  | x :: xs' ->
      (match p x with
        Some(r) -> r :: (filter_map p xs')
      | None -> filter_map p xs')

let rec filter_map2 p xs ys =
  match xs, ys with
    [], [] -> []
  | x :: xs', y :: ys' ->
      (match p x y with
        Some(r) -> r :: (filter_map2 p xs' ys')
      | None -> filter_map2 p xs' ys')
  | _ -> assert false

let filter_maplac f xs =
  let rec aux acs xs =
    match xs with
      [] -> acs
    | x :: xs' ->
        (match f acs x xs' with
          None ->
            aux acs xs'
        | Some(x') ->
            aux (acs @ [x']) xs')
  in
  aux [] xs

(** {5 Iteration} *)

let rec pr_list epr sep ppf xs =
  match xs with
    [] ->
      ()
  | [x] ->
      Format.fprintf ppf "%a" epr x
  | x :: xs' ->
      Format.fprintf
        ppf
        "%a%a%a"
        epr x
        (fun ppf sep -> Format.fprintf ppf sep) sep
        (pr_list epr sep) xs'

let rec iter3 f xs ys zs =
  match xs, ys, zs with
    [], [], [] ->
      ()
  | x :: xs', y :: ys', z :: zs' ->
      let () = f x y z in
      iter3 f xs' ys' zs'
  | _ -> assert false

(** {5 Query} *)

let rec is_prefix xs ys =
  match xs, ys with
    [], _ -> true
  | x :: xs', y :: ys' -> x = y && is_prefix xs' ys'
  | _, _ -> false

let all_equiv eqrel xs =
  match xs with
    [] ->
      true
  | x :: xs ->
      List.for_all (fun x' -> eqrel x x') xs

(** @require xs is not empty
    @return xs without the last element *)
let rec init xs =
  match xs with
    [] -> assert false
  | [x] -> []
  | x :: xs' -> x :: (init xs')

(** @deprecated use ExtList.List.last *)
let rec last xs =
  match xs with
    [] -> assert false
  | [x] -> x
  | x :: xs' -> last xs'

let elem_of_singleton xs =
  match xs with
    [x] -> x
  | _ ->
      let _ = Format.printf "%d@," (List.length xs) in
      assert false

let rec remove_one p xs =
  match xs with
    [] ->
      assert false
  | x :: xs' ->
      if p x then
        xs'
      else
        x :: remove_one p xs'

let filteri p xs =
  let rec aux i xs =
    match xs with
      [] -> []
    | x :: xs' ->
        if p i x then
          x :: (aux (i + 1) xs')
        else
          aux (i + 1) xs'
  in
  aux 0 xs

let zip = List.combine

let rec zip3 xs ys zs =
  match xs, ys, zs with
    [], [], [] ->
      []
  | x :: xs', y :: ys', z :: zs' ->
      (x, y, z) :: zip3 xs' ys' zs'
  | _ -> assert false

let unzip = List.split

let rec unzip3 ls =
  match ls with
    [] ->
      [], [], []
  | (x, y, z) :: ls ->
      let (xs, ys, zs) = unzip3 ls in
      x :: xs, y :: ys, z :: zs

let sort_by f xs =
  List.map
    snd
    (List.sort
      ~cmp:(fun (n1, _) (n2,_) -> n1 - n2)
      (List.map (fun x -> f x, x) xs))

let sort_by_dec f xs =
  List.map
    snd
    (List.sort
      ~cmp:(fun (n1, _) (n2,_) -> n2 - n1)
      (List.map (fun x -> f x, x) xs))

(** nonemp_prefixes \[1;2;3\] = \[\[1\]; \[1; 2\]; \[1; 2; 3\]\] *)
let nonemp_prefixes ts =
  let _, tss = List.fold_left
    (fun (ts, tss) t ->
      ts @ [t], tss @ [ts @ [t]])
    ([], [])
    ts
  in
  tss

(** @return a minimal subset of xs that satisfy p
    @require p xs *)
let minimal p xs =
  let rec aux xs ys =
    match xs with
      [] -> ys
    | x :: xs' ->
        if p (xs' @ ys) then
          aux xs' ys
        else
          aux xs' (x :: ys)
  in
  aux xs []

let rec classify eqrel xs =
  match xs with
    [] -> []
  | x :: xs' ->
      let t, f = List.partition (fun x' -> eqrel x x') xs' in
      (x :: t) :: (classify eqrel f)

(** @deprecated use ExtList.List.split_nth *)
let rec split_nth n xs =
  if n = 0 then
    [], xs
  else if n > 0 then
    (match xs with
      x :: xs' ->
        let xs1, xs2 = split_nth (n - 1) xs' in
        x :: xs1, xs2
    | _ -> assert false)
  else
    assert false

let rec split_at ls xs =
  match ls with
    [] -> [xs]
  | l :: ls ->
      let xs1, xs2 = List.split_nth l xs in
      xs1 :: split_at ls xs2

let rec replace_with_hole i xs =
  match xs with
    [] -> assert false
  | x :: xs' ->
      if i = 0 then
        fun ys -> ys @ xs'
      else if i > 0 then
        let ctx = replace_with_hole (i - 1) xs' in
        fun ys -> x :: ctx ys
      else
        assert false

let rec pick p xs =
  match xs with
    [] -> raise Not_found
  | x :: xs' ->
      if p x then
        [], x, xs'
      else
        let ls, y, rs = pick p xs' in
        x :: ls, y, rs

let rec find_app f xs =
  match xs with
    [] ->
      raise Not_found
  | x :: xs' ->
      try
        f x
      with Not_found ->
        find_app f xs'

(** {5 Transformation} *)

let rec map3 f xs ys zs =
  match xs, ys, zs with
    [], [], [] ->
      []
  | x :: xs', y :: ys', z :: zs' ->
      f x y z :: map3 f xs' ys' zs'
  | _ ->
      assert false

let mapctx f xs =
  List.mapi (fun i x -> f (replace_with_hole i xs) x) xs

let maplr f xs =
  let rec aux ls rs =
    match rs with
      [] -> []
    | x :: rs ->
        f ls x rs ::
        aux (ls @ [x]) rs
  in
  aux [] xs

let maplac f xs =
  let rec aux acs xs =
    match xs with
      [] -> acs
    | x :: xs' ->
        let x' = f acs x xs' in
        aux (acs @ [x']) xs'
  in
  aux [] xs

let maprac f xs =
  let rec aux xs acs =
    match xs with
      [] -> acs
    | x :: xs' ->
        let x' = f (List.rev xs') x acs in
        aux xs' (x' :: acs)
  in
  aux (List.rev xs) []

(** @param xs \[x1; ... ; xm\]
    @param ys \[y1; ... ; yn\]
    @return \[f x1 y1; ... ; f x1 yn; ... ; f xm y1; ... ; f xm yn\] *)
let multiply_list f xs ys =
  concat_map
    (fun x ->
      List.map (fun y -> f x y) ys)
    xs

let product_list f xss =
  if xss = [] then
    assert false
  else
    List.fold_left
      (multiply_list f)
      (List.hd xss)
      (List.tl xss)

let foldac_left f z xs =
  let rec aux acs z xs =
    match xs with
      [] -> acs, z
    | x :: xs' ->
        let x', z' = f acs z x xs' in
        aux (acs @ [x']) z' xs'
  in
  aux [] z xs

let foldac_right f xs z =
  let rec aux xs z acs =
    match xs with
      [] -> z, acs
    | x :: xs' ->
        let z', x' = f (List.rev xs') x z acs in
        aux xs' z' (x' :: acs)
  in
  aux (List.rev xs) z []

(** {5 Generation} *)

let rec unfold f seed =
  match f seed with
    None -> []
  | Some(x, seed') ->
      x :: unfold f seed'

(** {6 Functions on sets} *)

let rec power xs =
  match xs with
    [] ->
      [[]]
  | x :: xs' ->
      let xss = power xs' in
      concat_map (fun xs -> [xs; x :: xs]) xss

(** @return all the subsets of xs with the size n *)
let nsubsets n xs =
  let rec aux n yszss =
    if n = 0 then
      yszss
    else
      concat_map
        (fun (ys, zs) ->
          (** @invariant set_equiv (ys @ zs) xs *)
          maplr (fun ls y rs -> ls @ rs, y :: zs) ys)
        (aux (n - 1) yszss)
  in
  List.map snd (aux n [xs, []])

(*
let rec diff xs ys =
  match xs, ys with
    [], [] | [],  _ |  _, [] -> xs
  | x' :: xs', ys ->
      if List.mem x' ys then
        diff xs' ys
      else
        x' :: (diff xs' ys)
*)

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1
let subset l1 l2 = List.for_all (fun x -> List.mem x l2) l1
let inter l1 l2 = List.filter (fun x -> List.mem x l2) l1
let union l1 l2 = List.unique (l1 @ l2)
let set_equiv l1 l2 = subset l1 l2 && subset l2 l1
let intersects l1 l2 = List.exists (fun x -> List.mem x l2) l1
let is_dup ?(cmp = (=)) xs =
  List.length (List.unique ~cmp:cmp xs) <> List.length xs

let rec get_dup_elems ?(cmp = (=)) xs =
  match xs with
    [] ->
      []
  | x :: xs' ->
      let xs'' = List.filter (fun x' -> not (cmp x x')) xs' in
      if List.length xs' <> List.length xs'' then
        x :: get_dup_elems xs''
      else
        get_dup_elems xs'

(** divide xs by the transitive clousre of rel *)
let rec equiv_classes rel xs =
  match xs with
    [] ->
      []
  | x :: xs ->
      let aux xs1 xs2 =
        let ys1, ys2 =
          List.partition (fun y -> List.exists (rel y) xs1) xs2
        in
        xs1 @ ys1, ys2
      in
      let ls, rs =
        fixed_point
          (fun (xs, ys) -> aux xs ys)
          (fun (xs1, _) (xs2, _) -> List.length xs1 = List.length xs2)
          ([x], xs)
      in
      let ecs = equiv_classes rel rs in
      ls :: ecs

let rec representatives eqrel xs =
  match xs with
    [] -> []
  | x :: xs' ->
      x :: representatives eqrel (List.filter (fun y -> not (eqrel x y)) xs')

(** {6 Functions on multisets} *)

let rec diff_ms l1 l2 =
  match l1 with
    [] -> []
  | x :: l1' ->
      (try
        let l, _, r = pick (fun x' -> x = x') l2 in
        diff_ms l1' (l @ r)
      with Not_found ->
        x :: diff_ms l1' l2)

let rec subset_ms l1 l2 =
  match l1 with
    [] -> true
  | x :: l1' ->
      (try
        let l, _, r = pick (fun x' -> x = x') l2 in
        subset_ms l1' (l @ r)
      with Not_found ->
        false)

(** {6 Functions on maps} *)

let is_map f =
  not (is_dup (List.map fst f))

let dom f =
  List.map fst f


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

(** {6 Functions on bitvectors} *)

let bv_not bv =
  List.map
    (fun n -> if n = 0 then 1 else if n = 1 then 0 else assert false)
    bv

let bv_inc bv =
  let rec aux bv =
    match bv with
      [] -> assert false
    | 0 :: bv -> 1 :: bv
    | 1 :: bv -> 0 :: aux bv
  in
  List.rev (aux (List.rev bv))

let bv_dec bv =
  let rec aux bv =
    match bv with
      [] -> assert false
    | 0 :: bv -> 1 :: aux bv
    | 1 :: bv -> 0 :: bv
  in
  List.rev (aux (List.rev bv))

let bv_of_nat n =
  let _ = assert (n >= 0) in
  let rec aux bv n =
    if n = 0 then
      bv
    else
      aux (n mod 2 :: bv) (n / 2)
  in
  if n = 0 then
    [0]
  else
    aux [] n

let bv_of_int bits n =
  assert false
(*
  if n >= 0 then
    bv_of_nat bits n
  else
    bv_inc (bv_not (bv_of_nat bits (-n)))
*)

let nat_of_bv bv =
  List.fold_left (fun x y -> x * 2 + y) 0 bv

let int_of_bv bv =
  if List.hd bv = 0 then
    nat_of_bv bv
  else if List.hd bv = 1 then
    -nat_of_bv (bv_not (bv_dec bv))
  else
    assert false
