let rec insert (x:int) ys =
  match ys with
  | [] -> [x]
  | y::ys' ->
      if x < y then x::ys
      else y::(insert x ys')

let rec gen_smaller n =
  let m = Random.int 0 in
  if m < n
  then m
  else gen_smaller n

let rec make_sorted_list n =
  if n = 0
  then []
  else
    let xs = make_sorted_list (n-1) in
    match xs with
    | [] -> [Random.int 0]
    | x::xs' -> gen_smaller x :: x::xs'

let main n x i j =
  let xs = make_sorted_list n in
  let ys = insert x xs in
  if i < j
  then assert (List.nth ys i <= List.nth ys j)
