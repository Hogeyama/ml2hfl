let rec map (f:int -> int) (xs:int list) =
  match xs with
      [] -> []
    | x::xs' -> f x :: map f xs'

let rec length (xs:int list) =
  match xs with
      [] -> 0
    | _::xs' -> 1 + length xs'

let rec make_list n =
  if n < 0
  then []
  else n :: make_list (n-1)

let succ x = x + 1

let main n =
  let xs = make_list n in
  let xs' = map succ xs in
    assert (length xs = length xs')
