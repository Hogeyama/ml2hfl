
type rtree = Node of int * rtree list

let tree = Node(3, [Node(5, []); Node(7, [Node(11, [])])])

let rec sum tree =
  match tree with
  | Node(n,trees) -> List.fold_left (fun acc tree -> acc + sum tree) n trees

let main() = assert (sum tree >= 0)
