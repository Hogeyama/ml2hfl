let app p f g = f g
let f x k = k x
let check x y = assert (x = y)
let main a b = app (2 * a + b) (f (2 * a + b)) (check (2 * a + b))
