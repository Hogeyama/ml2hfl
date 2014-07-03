let rec append (xs, ys) =
  match xs with
    [] -> ys
  | x::xs' -> x :: append (xs', ys)

let rec make_list n =
  if n < 0
  then []
  else Random.int 0 :: make_list (n-1)

let rec list_eq xsys =
  match xsys with
    [], [] -> true
  | x::xs', y::ys' -> x = y && list_eq (xs', ys')
  | _ -> false

let main i n =
  let xs = make_list n in
  let ys = append(xs, []) in
  assert (List.nth ys i = List.nth xs i)
