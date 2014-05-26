let rec fib n =
  if n = 0 then
    0
  else if n = 1 then
    1
  else
    fib (n-1) + fib (n-2)

let rec fib' a b n =
  if n = 0 then
    a
  else if n = 1 then
    b
  else fib' b (a+b) (n-1)

let main a b n =
  if a = 0 && b = 1 then
  assert (fib n = fib' a b n)
