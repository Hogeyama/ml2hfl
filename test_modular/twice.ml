
let rec twice f x =
  f (f x)

let succ x = x + 1

let main n =
  assert (n <= twice succ n)
