let rec fib_e n =
  if n=0 then 1
  else if n=1 then 1
  else fib_e (n-1) + fib_e (n-2)
let main () =
  let r = read_int () in fib_e r; assert false
