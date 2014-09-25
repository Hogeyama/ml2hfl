
let rec sum n =
  if n <= 0
  then 0
  else n + sum (n-1)

let delta f n =
  f n - f (n-1)

let main n =
  assert (delta sum n >= 0)
