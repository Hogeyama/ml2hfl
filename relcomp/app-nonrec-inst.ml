let apply ex f x = f x
let check x y = assert (x = y)
let main (n:int) = apply n (check n) n
