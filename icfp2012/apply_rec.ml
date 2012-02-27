(* apply_rec.ml: verification succeeded *)
let apply a f x = f x
let check (x:int) y = assert (x = y)
let rec loop n = apply (ExtFuncs.f n) (check n) n; (loop (n + 1) : int)
let main (x:int) = loop x (* if 0 is passed instead of x, then MoCHi does not terminate *)
