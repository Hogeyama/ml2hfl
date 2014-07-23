let rec make_list n =
  if n < 0
  then []
  else Random.int 0 :: make_list (n-1)

let rec append (xs, ys) =
  match xs with
    [] -> ys
  | x::xs' ->
      let rs = append (xs', ys) in
      let rs' = x :: rs in
      rs'

let main i n =
  let xs = make_list n in
  let ys = append(xs, []) in
  assert (List.nth ys i = List.nth xs i)
