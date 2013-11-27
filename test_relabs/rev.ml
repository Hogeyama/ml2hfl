let rec list_eq xs ys =
  match xs, ys with
    0, 0 -> true
  | x::xs', y::ys' -> x = y && list_eq xs' ys'
  | _ -> false

let rec rev acc xs =
  match xs with
    [] -> acc
  | x::xs -> rev (x::acc) xs

let main xs =
  let ys = rev xs in
  let zs = rev ys in
  assert (list_eq xs zs)
