let rec append xs ys =
  match xs with
    [] -> ys
  | x::xs' -> x :: append xs' ys

let rec make_list n =
  if n < 0
  then []
  else Random.int 0 :: make_list n

let rec list_eq xs ys =
  match xs, ys with
    0, 0 -> true
  | x::xs', y::ys' -> x = y && list_eq xs' ys'
  | _ -> false

let main n =
  let xs = make_list n in
  assert (list_eq xs (append [] xs))
