let f x y = assert (not ((x () > 0) && (y () <= 0)))
let h (x:int) (y:unit) = x
let rec g n = f (h n) (h n)
let main m = g m
