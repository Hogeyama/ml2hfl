(* verification succeeded *)
let app f g = f g
let f x k = k x
let check x y = assert (x = y)
let main a b = app (f (2 * a + 3 * b)) (check (2 * a + 3 * b))
