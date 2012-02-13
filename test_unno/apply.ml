let apply a f x = f x
let check (x:int) y = assert (x = y)
let main (n:int) = apply (ApplyRelComp.f n) (check n) n
