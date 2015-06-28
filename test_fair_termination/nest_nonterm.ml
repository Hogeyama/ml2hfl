let rec f m n =
  if n < 0 then
    if m < 0 then ()
    else
      let _ = event "B" in    
      f (m+1) m
  else
    let _ = event "A" in    
    f m (n-1)
let main m n = f m n

(*{SPEC}
  fairness: (Call, A)
  fairness: (B, Never)
{SPEC}*)
