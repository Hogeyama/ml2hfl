let rec make_list m =
  if m <= 0
  then []
  else Random.int 0 :: make_list (m-1)

let abs x =
  if x < 0
  then - x
  else x

let rec sum f xs =
  match xs with
      [] -> 0
    | x::xs' -> f x + sum f xs'

let main m =
  let xs = make_list m in
    assert (sum abs xs >= 0)
