let rec sum n =
  if n >= 0 then 0
  else if n < 0 then
    let _ = event "A" in    
    n + sum (n - 1)
  else n + sum (n - 1)
let main r = sum r

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
