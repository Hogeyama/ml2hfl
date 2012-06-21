let apply f x = f x
let check x y = assert (x = y)
let main (n:int) = apply (check n) n
