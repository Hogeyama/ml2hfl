let rec f n =
  if n<0 then ()
  else
    let r = Random.int 0 in
    if r>0 then
      f (n-1)
    else
      let _ = event "A" in
      f (n+1)
let main r = f r

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
