let app p f x = f x
let check x y = assert (x = y)
let main a b = app (2 * a + b) (check (2 * a + b)) (2 * a + b)
