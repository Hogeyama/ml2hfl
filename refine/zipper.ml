open ExtList
open ExtString

(** Zippers *)

(** {6 Types} *)

type 'a tree = Node of 'a * 'a tree list
type 'a path = Top | Path of 'a path * 'a tree list * 'a * 'a tree list
type 'a location = Loc of 'a tree * 'a path

(** {6 Functions on trees} *)

let make nd cs = Node(nd, cs)
let get (Node(nd, _)) = nd
let children (Node(_, trs)) = trs
let set (Node(_, trs)) nd = Node(nd, trs)

let rec nodes_of_tree (Node(nd, trs)) =
  nd::Util.concat_map nodes_of_tree trs

(** {6 Functions on paths} *)

let rec nodes_of_path p =
  match p with
    Top -> []
  | Path(up, trs1, nd, trs2) ->
      nodes_of_path up @ Util.concat_map nodes_of_tree trs1 @ [nd] @ Util.concat_map nodes_of_tree trs2

(** {6 Functions on zippers} *)

let zipper tr = Loc(tr, Top)
let up (Loc(tr, p)) =
  match p with
    Top -> raise Not_found
  | Path(up, trs1, nd, trs2) -> Loc(Node(nd, trs1 @ tr::trs2), up)
let down (Loc(tr, p)) cond =
  match tr with
    Node(nd, trs) ->
      let trs1, tr', trs2 =
        try
          Util.pick (fun tr -> cond (get tr)) trs
        with Not_found ->
          assert false
      in
      Loc(tr', Path(p, trs1, nd, trs2))
let rec root (Loc(tr, p) as l) =
  match p with
    Top -> tr
  | _ -> root (up l)
let insert_down (Loc(Node(nd, trs), p)) tr =
  Loc(tr, Path(p, trs, nd, []))

(** {6 Functions on trees using zippers} *)

let find_leaf tr =
  let rec aux (Loc(Node(nd, trs), p) as loc) =
    match List.rev trs with
      [] ->
        loc
    | tr'::trs' ->
        aux (Loc(tr', Path(p, List.rev trs', nd, [])))
  in
  aux (zipper tr)

let find_leaves tr =
  let rec aux (Loc(Node(nd, trs), p) as loc) =
    match trs with
      [] ->
        [loc]
    | _ ->
        List.concat
		        (List.init
            (List.length trs)
		          (fun i ->
		            let trs1, tr::trs2 = List.split_nth i trs in
		            aux (Loc(tr, Path(p, trs1, nd, trs2)))))
  in
  aux (zipper tr)

let find_rev_leaf tr =
  let rec aux (Loc(Node(nd, trs), p) as loc) =
    match trs with
      [] ->
        loc
    | tr'::trs' ->
        aux (Loc(tr', Path(p, [], nd, trs')))
  in
  aux (zipper tr)

let find_all cond tr =
  let rec aux (Loc(Node(nd, trs), p) as loc) =
    if cond nd then [loc] else [] @
    Util.concat_map
      (fun tr -> aux (down loc (fun nd -> nd = get tr(*???*))))
      trs
  in
  aux (zipper tr)

