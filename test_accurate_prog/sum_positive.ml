let rec sum n =
  if n <= 0
  then 0
  else n + sum (n-1)

let main n =
  if n >= 1
  then assert (sum n = n + sum (n-1))