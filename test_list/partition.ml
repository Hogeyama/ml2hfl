let rec length xs =
  match xs with
      [] -> 0
    | _::xs' -> 1 + length xs'

let rec make_list n =
  if n = 0
  then []
  else Random.int 10 :: make_list (n-1)

let rec partition f xs =
  match xs with
  | [] -> [],[]
  | x::xs' ->
      let ys1,ys2 = partition f xs' in
      if f x
      then x::ys1, ys2
      else ys1, x::ys2

let main n =
  let f x = Random.bool () in
  let xs = make_list n in
  let ys1,ys2 = partition f xs in
  let l = length xs in
  let l1 = length ys1 in
  let l2 = length ys2 in
  assert (l = l1 + l2)
