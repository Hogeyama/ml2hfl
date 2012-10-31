let rec sum n =
  if n <= 0
  then 0
  else n + sum (n-1)
in
let f n = n in
let n = f ?n? in
  if 0 <= sum n
  then ()
  else fail ()
