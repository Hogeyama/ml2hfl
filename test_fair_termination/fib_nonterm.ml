let rec fib_e n =
  if n=0 then 1
  else if n=1 then 1
  else if n<0 then
    let _ = event "A" in    
    fib_e (n-1) + fib_e (n-2)
  else
    fib_e (n-1) + fib_e (n-2)
let main r = fib_e r

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
