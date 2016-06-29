
let rec fsum f n =
  if n <= 0
  then 0
  else f n + fsum f (n-1)

let rec double x =
  if Random.bool()
  then x+x
  else 2 + double (x-1)

let main n =
  assert (n <= fsum double n)
