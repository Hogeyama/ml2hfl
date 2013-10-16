let rec list_eq xs ys =
  match xs, ys with
    0, 0 -> true
  | x::xs', y::ys' -> x = y && list_eq xs' ys'
  | _ -> false

let rec rev acc xs =
  match xs with
    [] -> acc
  | x::xs -> rev (x::acc) xs

let rev_rev xs ys =
  rev xs, rev ys

let main xs =
  let ys = rev xs in
  let zs = rev_rev xs xs in
  assert (list_eq xs ys)
