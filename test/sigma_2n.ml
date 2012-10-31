
let rec f n =
  if n <= 0
  then 1
  else 2 + f (n-1)
in
let rec sigma f n =
  if n <= 0
  then 0
  else f n + sigma f (n-1)
in
  assert (sigma f n >= n)


