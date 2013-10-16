let rec sum f n =
  if n < 0
  then 0
  else f n + sum f (n-1)

let rec fib n =
  if n <= 0 then
    0
  else if n = 1 then
    1
  else
    fib (n-1) + fib (n-2)

let rec main n =
  assert (sum fib n <= sum fib (n+1))
