let app ex f g = f g
let f x k = k x
let check x y = assert (x = y)
let main a b = app (4 * a + 2 * b) (f (4 * a + 2 * b)) (check (4 * a + 2 * b))
