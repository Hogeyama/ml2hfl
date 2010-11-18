let max max2 x y z =
  max2 (max2 x y) y
in
let f x y =
  if x >= y
  then x
  else y
in
let m = max f x y z in
  assert (f x m = m && f y m = m && f z m = m)

