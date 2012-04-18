(* verification succeeded *)
let app f g = f g
let f x k = k x
let check x y = assert (x = y)
let main a b = app (f (4 * a + 2 * b)) (check (4 * a + 2 * b))
