let app (*ex*) f x = f x
let check x y = assert (x = y)
let main a b = app (*(4 * a + 2 * b)*) (check (4 * a + 2 * b)) (4 * a + 2 * b)
