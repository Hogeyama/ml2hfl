(* verification failed *)
let app f x = f x
let check x y = assert (x = y)
let main (a:int) (b:int) = app (check (a+b)) (a+b)
