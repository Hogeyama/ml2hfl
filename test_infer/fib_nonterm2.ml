let rec fib_e n =
  if n=0 then 1
  else fib_e (n-1) + fib_e (n-2)
let main () =
  let r = read_int () in 
 (if r>0 then fib_e r else -1); assert false
