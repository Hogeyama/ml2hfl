let rec append xs1 xs2 =
  match xs1 with
      [] -> xs2
    | x::xs1' -> x :: append xs1' xs2

let rec length xs =
  match xs with
      [] -> 0
    | _::xs' -> 1 + length xs'

let rec make_list n =
  if n = 0
  then []
  else n :: make_list (n-1)

let main n m =
  let xs = make_list n in
  let ys = make_list m in
    assert (length (append xs ys) = length xs + length ys)
