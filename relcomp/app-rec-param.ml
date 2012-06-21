let apply a f x = f x
let check x y = assert (x = y)
let rec loop n : unit = apply (ExtFuncs.f1_1 n) (check n) n; loop (n + 1)
let main (x:int) = loop x
