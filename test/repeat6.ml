let succ x = x + 1 in
let rec repeat n =
  if n = 0
  then 0
  else succ (repeat (n-1))
in
  assert (repeat n >= n)
