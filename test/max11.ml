let max x y =
  if x >= y then x else y
in
let m1 = max x y in
let m2 = if y >= m1 then y else m1 in
  assert (m2 = m1)
