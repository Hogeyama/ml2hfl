(*
let rec add x1 x2 =
  if x1 = 0 then
    x2
  else
    1 + add (x1 - 1) x2
*)
let add x1 x2 = x1 + x2
let twice f x = f (f x)
let main n = assert (twice (add n) 0 >= 2 * n)
