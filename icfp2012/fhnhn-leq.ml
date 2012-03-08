(* fhnhn.ml: safe but ill-typed *)
let f x y = assert (not ((x () > 0) && (y () <= 0)))
let h (x:int) (y:unit) = x
let main n = f (h n) (h n)
