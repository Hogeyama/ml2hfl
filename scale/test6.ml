let rec map f xs =
  match xs with
      [] -> []
    | x::xs' -> f x :: map f xs'

let rec length (xs:int list) =
  match xs with
      [] -> 0
    | _::xs' -> 1 + length xs'

let rec make_list n =
  if n < 0
  then []
  else n :: make_list (n-1)

let rec sum x =
  if x < 0
  then 0
  else x + sum (x-1)

let succ x = x + 1

let prec x = x - 1

let rec iter f xs =
  match xs with
    [] -> ()
  | x::xs' -> f x; iter f xs'

let rec append xs ys =
  match xs with
    [] -> ys
  | x::xs' -> x :: append xs' ys

let rec flatten xss =
  match xss with
    [] -> []
  | xs::xss' -> append xs (flatten xss')

let main n =
  let xs = make_list n in
  let xs = map make_list xs in
  let xs = flatten xs in
  iter (fun x -> assert (x >= 0)) xs
