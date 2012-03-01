(* fhnhneq_inst.ml: verification failed *)
let f a x y = assert (x () = y ())
let h (x:int) (y:unit) = x
let fex n = 2 * n
let main n = f (fex n) (h n) (h n)
