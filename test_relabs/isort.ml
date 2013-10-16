let rec insert (x:int) ys =
  match ys with
    | [] -> [x]
    | y::ys' ->
        if x < y then x::y::ys'
        else y::(insert x ys')

let rec insertsort xs =
  match xs with
    | [] -> []
    | x::xs' ->
        insert x (insertsort xs')

let rec make_list n =
  if n = 0
  then []
  else n :: make_list (n-1)

let rec check (xs:int list) =
  match xs with
      [] -> true
    | x::[] -> true
    | x1::x2::xs' -> x1 <= x2 && check (x2::xs')

let main n =
  let xs = make_list n in
    assert (check (insertsort xs))
