
let rec make_list m =
  if m <= 0
  then [100]
  else Random.int 0 :: make_list (m-1)

let rec make_list_list m =
  if m <= 0
  then []
  else make_list (Random.int 0) :: make_list_list (m-1)

let head = function
    [] -> assert false
  | x::xs -> x

let rec map f = function
    [] -> []
  | x::xs -> f x :: map f xs

let main m = map head (make_list_list m)
