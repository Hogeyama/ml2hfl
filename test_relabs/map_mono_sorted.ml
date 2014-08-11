let rec map f xs =
  match xs with
  | [] -> []
  | x::xs' -> f x :: map f xs'

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

let double x = x + x

let main n x i j =
  let xs = make_sorted_list n in
  let ys = List.map double xs in
  if i < j
  then assert (List.nth ys i <= List.nth ys j)
