(* safe but untypable *)
let f x y = assert (x () = y ())
let h (x:int) (*()*)u = x
let main n = f (h n) (h n)
