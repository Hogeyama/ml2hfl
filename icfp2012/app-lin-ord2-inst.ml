(* verification succeeded *)
let app ex f x = f x
let check x y = assert (x = y)
let main a b = app (2 * a + 3 * b) (check (2 * a + 3 * b)) (2 * a + 3 * b)
