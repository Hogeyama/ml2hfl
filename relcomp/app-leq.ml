let app (*ex*) f x = f x
let check x y = assert (x = y)
let main (a:int) b = app (*(a - b)*) (check (a <= b)) (a <= b)
