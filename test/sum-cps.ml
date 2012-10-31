let rec sum n k =
  if n <= 0
  then k 0
  else sum (n-1) (let f r = k (n + r) in f)
in
let c x y =
  if x <= y
  then ()
  else fail ()
in
let f n = n in
let n = f ?n? in
  sum n (c n)
