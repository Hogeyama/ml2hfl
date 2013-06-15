type tree = Leaf | Node of int * tree * tree

let rec forall f = function
    Leaf -> true
  | Node(n,t1,t2) -> f n && forall f t1 && forall f t2

let rec make_tree n =
  if n <= 0
  then Leaf
  else Node(n, make_tree (n-1), make_tree (n-1))

let main n =
  assert (forall (fun n -> n > 0) (make_tree n))
