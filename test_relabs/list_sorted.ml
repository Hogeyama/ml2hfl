let rec make_list n =
  if n < 0
  then []
  else -n :: make_list (n-1)

let rec sorted (xs:int list) =
  match xs with
      [] -> true
    | x::[] -> true
    | x1::x2::xs' -> x1 <= x2 && sorted (x2::xs')

let main n =
  let xs = make_list n in
  assert (sorted xs)
