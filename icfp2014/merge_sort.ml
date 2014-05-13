let rec merge xs ys =
  match xs, ys with
  | [], _ -> ys
  | _, [] -> xs
  | x::xs', y::ys' -> if x < y then x :: merge xs' ys else y :: merge xs ys'

let rec is_sorted xs =
  match xs with
  | [] -> true
  | [_] -> true
  | x1::x2::xs -> x1 <= x2 && is_sorted (x2::xs)

let rec divide xs =
  match xs with
  | [] -> [], []
  | [x] -> [x], []
  | x1::x2::xs' ->
      let xs1,xs2 = divide xs' in
      x1::xs1, x2::xs2

let rec merge_sort xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | _ ->
      let xs1,xs2 = divide xs in
      let xs1' = merge_sort xs1 in
      let xs2' = merge_sort xs2 in
      merge xs1' xs2'

let rec make_list n =
  if n < 0
  then []
  else Random.int 0 :: make_list n

let main n =
  let xs = make_list n in
  let xs' = merge_sort xs in
  assert (is_sorted xs')
