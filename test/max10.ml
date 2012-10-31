let max x y z =
  if x >= y
  then if x >= z then x else z
  else if y >= z then y else z
in
let m1 = max x y z in
let m2 = if y >= m1 then y else m1 in
  assert (m2 = m1)
