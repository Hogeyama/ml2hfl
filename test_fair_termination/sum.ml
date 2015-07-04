(*
USED: PLDI2011 as sum
USED: PEPM2013 as sum
*)
let rec sum s f n =
  if n <= 0
  then f s
  else f n + sum s f (n-1)

let f n = n

let main n =
  assert (n <= sum 0 f n)

let _ = main (Random.int 0)
