type tree = Leaf | Node of int * tree * tree

let rec insert n t =
  match t with
    Leaf -> Node(n, Leaf, Leaf)
  | Node(m,t1,t2) ->
    if n = m then
      Node(m, t1, t2)
    else if n < m then
      Node(m, insert n t1, t2)
    else
      Node(m, t1, insert n t2)

let rec inserts ns t =
  match ns with
    [] -> t
  | n::ns' -> inserts ns' (insert n t)

let rec make_list n =
  if n = 0
  then []
  else Random.int 256 :: make_list (n-1)

let rec check t =
  match t with
    Leaf -> true
  | Node(n,t1,t2) ->
    let b1 =
      match t1 with
        Leaf -> true
      | Node(m,t11,t12) -> m < n
    in
    let b2 =
      match t2 with
        Leaf -> true
      | Node(m,t21,t22) -> n < m
    in
    b1 && b2 && check t1 && check t2

let main n =
  let ns = make_list n in
  let t = inserts ns Leaf in
  assert (check t)
