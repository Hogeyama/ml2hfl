let f ex x y = assert (not ((x () > 0) && (y () <= 0)))
let h (x:int) u = x
let main n = f (ExtFuncs.f1_1 n) (h n) (h n)
