(* fhnhn-param.ml: ??? *)
let f a x y = assert (not ((x () > 0) && (y () <= 0)))
let h (x:int) (y:unit) = x
let main n = f (ExtFuncs.f1_1 n) (h n) (h n)
