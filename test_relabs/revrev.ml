let rec rev n i acc xs =
  match xs with
    [] -> acc
  | x :: xs' -> rev n (i-1) (x::acc) xs'
let rev n xs = rev n n [] xs

let rec list_eq (xs, ys) =
  match xs, ys with
    [], [] -> true
  | x::xs', y::ys' -> x = y && list_eq (xs', ys')
  | _ -> false

let rec make_list n =
  if n < 0
  then []
  else Random.int 0 :: make_list (n-1)

let main n =
  let xs = make_list n in
  assert (list_eq (rev n (rev n xs), xs))
