(* fhnhneq.ml: safe but ill-typed *)
let f x y = assert (x () = y ())
let h (x:int) (y:unit) = x
let main n = f (h n) (h n)
