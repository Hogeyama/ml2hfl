
let max x y z max2 =
  max2 (max2 x y) z
in
let f x y =
  if x >= y
  then x
  else y
in
let m = max x y z f in
  assert (f m y = m)
