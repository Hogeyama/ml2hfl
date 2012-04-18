(* verification succeeded *)
let app f x = f x
let check x y = assert (x = y)
let main (a:int) b = app (check (a <= b)) (a <= b)
