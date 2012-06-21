let app f x = f x
let check x y = assert (x = y)
let main (a:int) (b:int) = app (check (4 * a + 2 * b)) (4 * a + 2 * b)
