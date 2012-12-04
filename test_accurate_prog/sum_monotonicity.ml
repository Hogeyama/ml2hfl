let rec sum n =
  if n <= 0
  then 0
  else n + sum (n-1)

let main x y =
  if x <= y then assert (sum x <= sum y)
