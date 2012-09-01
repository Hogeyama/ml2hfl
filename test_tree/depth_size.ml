type tree = Leaf | Node of tree * tree

let rec size t =
  match t with
      Leaf -> 1
    | Node(t1,t2) -> 1 + size t1 + size t2

let rec depth t =
  match t with
      Leaf -> 0
    | Node(t1,t2) -> 1 + depth t1 + depth t2

let main t =
  assert (depth t < size t)
