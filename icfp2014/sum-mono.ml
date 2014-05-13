
let rec sum n =
  if n <= 0
  then 0
  else n + sum (n - 1)

let main n m =
  if n < m
  then assert (sum n <= sum n)
