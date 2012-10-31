let max max x y = max x y
let f1 x y = if x >= y then x else y
let f2 x y = if x >= y then x else y
let main (x:int) y =
  let m = max f1 x y in
    assert (f2 x m = m)
