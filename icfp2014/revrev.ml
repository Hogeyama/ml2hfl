let rec rev acc xs =
  match xs with
    [] -> acc
  | x :: xs' -> rev (x::acc) xs'
let rev xs = rev [] xs

let rec list_eq xs ys =
  match xs, ys with
    0, 0 -> true
  | x::xs', y::ys' -> x = y && list_eq xs' ys'
  | _ -> false

let rec make_list n =
  if n < 0
  then []
  else Random.int 0 :: make_list n

let main n =
  let xs = make_list n in
  assert (list_eq xs (rev (rev xs)))
