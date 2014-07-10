let rec id x =
  if x < 0
  then 0
  else 1 + id (x-1)
let rec succ x =
  if x < 0
  then 1
  else 1 + succ (x - 1)
let rec double x =
  if x < 0
  then 0
  else 2 + double (x - 1)

(*
let main x =
  assert (id x + succ x = double x + 1)
*)

let eq x (f, g, h) =
  f x + g x = h x + 1

let main n =
  assert (eq n (id, succ, double))
