let f ex x y = assert (x () = y ())
let h (x:int) (y:unit) = x
let main n = f n (h n) (h n)
