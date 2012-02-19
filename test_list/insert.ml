let rec make_list m =
  if m <= 0
  then []
  else Random.int 0 :: make_list (m-1)

let rec length xs =
  match xs with
      [] -> 0
    | _::xs' -> 1 + length xs'

let rec insert (x:int) ys =
  match ys with
    | [] -> [x]
    | y::ys' ->
        if x < y then x::y::ys'
        else y::(insert x ys')

let main n m =
  let xs = make_list m in
  let xs' = insert n xs in
    assert (1 + length xs = length xs')
