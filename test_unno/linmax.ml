let max max21 max22 x y z = max22 (max21 x y) z
let f1 x y = if x >= y then x else y
let f2 x y = if x >= y then x else y
let f3 x y = if x >= y then x else y
let main (x:int) y z =
  let m = max f1 f2 x y z in
    assert (f3 x m = m)
