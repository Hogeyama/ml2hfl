let rec list_eq (xs, ys) =
  match xs, ys with
    [], [] -> true
  | x::xs', y::ys' -> x = y && list_eq (xs', ys')
  | _ -> false

let rec make_list n =
  if n < 0
  then []
  else Random.int 0 :: make_list (n-1)


let rec snoc xs x =
  match xs with
  | [] -> [x]
  | x'::xs' -> x' :: snoc xs' x

let rec rev1 xs =
  match xs with
  | [] -> []
  | x::xs' -> snoc (rev1 xs) x

let rec rev_append xs ys =
  match xs with
  | [] -> ys
  | x::xs' -> rev_append xs (x::ys)

let rev2 xs = rev_append xs []

let main n i =
  let xs = make_list n in
  let xs1 = rev1 xs in
  let xs2 = rev2 xs in
  assert (List.nth xs1 i = List.nth xs2 i)
