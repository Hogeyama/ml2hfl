let max max2 x y z = max2 (max2 x y) z
let f x y = if x >= y then x else y
let main (x:int) y z =
  let m = max f x y z in
  assert (f x m = m)
