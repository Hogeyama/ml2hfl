let max1 (x:int) (y:int) (z:int) max2 = 
	max2 (max2 x y) z
let f x y : int = if x >= y then x else y
let main (x:int) y z =
  let m = max1 x y z f in
  assert (f x m = m)
	
