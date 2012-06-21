let f a x y = assert (x () = y ())
let h (x:int) (y:unit) = x
let main n = f (ExtFuncs.f1_1 n) (h n) (h n)
