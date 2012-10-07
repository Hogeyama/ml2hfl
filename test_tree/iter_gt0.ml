type tree = Leaf | Node of int * tree * tree

let rec iter f = function
    Leaf -> ()
  | Node(n,t1,t2) -> f n; iter f t1; iter f t2

let rec make_tree n =
  if n <= 0
  then Leaf
  else Node(n, make_tree (n-1), make_tree (n-1))

let main n = iter (fun n -> assert (n > 0)) (make_tree n)
