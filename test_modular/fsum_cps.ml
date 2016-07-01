
let rec fsum f n k =
  if n <= 0
  then k 0
  else fsum f (n-1) (fun r -> f n (fun r' -> k (r'+r)))

let rec double x k =
  if Random.bool()
  then k (x+x)
  else double (x-1) (fun r -> k (2+r))

let main n =
  fsum double n (fun r -> assert (n <= r))
