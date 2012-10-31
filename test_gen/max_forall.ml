let rec for_all f (xs:int list) =
  match xs with
      [] -> true
    | x::xs' ->
        f x && for_all f xs'

let rec max m (xs:int list) =
  match xs with
    [] -> m
  | x::xs' ->
      let m' = if x > m then x else m in
      max m' xs'

let rec make_list n =
  if n < 0
  then []
  else n :: make_list (n-1)

let main n =
  let xs = make_list n in
  let m = max (-1) xs in
  assert (for_all (fun x -> x <= m) xs)
