(*let rec fib n =
  if n<2 then 1
  else fib (n-1) + fib (n-2)
in
  assert (fib n >= n)*)
let rec fib n =
  if n>=0 && n<=0 then 1
  else if n>=1 && n<=1 then 1
  else fib (n-1) + fib (n-2)
in assert (fib 3 >= 4)
