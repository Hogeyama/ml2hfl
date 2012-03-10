(* safe but untypable *)
let f x y = assert (x () = y ())
let h (x:int) () = x
let main n = f (h n) (h n)
