let apply a f x = f x
let check (x:int) y = assert (x = y)
let main (n:int) = apply (ExtFuncs.f n) (check n) n
