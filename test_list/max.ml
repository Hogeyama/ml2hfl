
let rec max_list (acc:int) xs =
  match xs with
      [] -> acc
    | x::xs' ->
        if x > acc
        then max_list x xs'
        else max_list acc xs'

let rec for_all f (xs:int list) =
  match xs with
      [] -> true
    | x::xs' ->
        f x && for_all f xs'

let rec make_list n =
  if n < 0
  then []
  else n :: make_list (n-1)

let main n =
  let xs = make_list n in
  let m = max_list 0 xs in
    assert (for_all (fun x -> x <= m) xs)



