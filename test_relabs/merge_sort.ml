let rec merge xs ys =
  match xs with
    [] -> ys
  | x::xs' ->
    match ys with
      [] -> xs
    | y::ys' ->
      if x <= y
      then x :: merge xs' ys
      else y :: merge xs ys'

let rec split xs =
  match xs with
    [] -> [],[]
  | [x] -> [x],[]
  | x::y::xs' ->
    let ys,zs = split xs' in
    x::ys, y::zs

let rec merge_sort xs =
  match xs with
    [] -> []
  | [x] -> [x]
  | _ ->
    let ys,zs = split xs in
    let ys' = merge_sort ys in
    let zs' = merge_sort zs in
    merge ys' zs'

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
    assert (check (merge_sort xs))
