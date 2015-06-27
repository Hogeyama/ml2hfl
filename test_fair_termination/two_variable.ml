let rec f m n =
  if m < n then
    f m (n-2)
  else if m > n then
    let _ = event "A" in    
    f m (n-2)
  else ()
let main m n =
  let m = read_int () in
  let n = read_int () in
  if m < n then f m n else ()

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
