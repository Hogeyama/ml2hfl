open ExtList

let rec prefix xs ys =
  match xs, ys with
    [], _ -> true
  | x::xs', y::ys' -> x = y && prefix xs' ys'
  | _, _ -> false

let rec fixed_point f eq x =
  let x' = f x in
  if eq x x' then x else fixed_point f eq x'

let rec init xs =
 match xs with
   [x] -> []
 | x::xs' -> x::(init xs')

(*
let rec last xs =
 match xs with
   [x] -> x
 | x::xs' -> last xs'
*)

let rec find_map f xs =
  match xs with
    [] -> raise Not_found
  | x::xs' ->
      try
        f x
      with Not_found ->
        find_map f xs'

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

let ctx_elem xs =
  List.mapi (fun i x -> ctx_of xs i, x) xs

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
        | _ -> failwith "Tools.partition_map")
  in
  aux [] [] (List.rev ys)

let rec split_at xs n =
  if n = 0 then
    [], xs
  else if n > 0 then
    (match xs with
      x::xs' ->
        let xs1, xs2 = split_at xs' (n - 1) in
        x::xs1, xs2)
  else
    assert false

let rec classify eqrel xs =
  match xs with
    [] -> []
  | x::xs' ->
      let t, f = List.partition (fun x' -> eqrel x x') xs' in
      (x::t)::(classify eqrel f)

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
