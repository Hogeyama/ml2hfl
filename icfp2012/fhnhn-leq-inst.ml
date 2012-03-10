(* fhnhn-inst.ml: verification succeeded *)
let f ex x y = assert (not ((x () > 0) && (y () <= 0)))
let h (x:int) (y:unit) = x
let main n = f n (h n) (h n)
