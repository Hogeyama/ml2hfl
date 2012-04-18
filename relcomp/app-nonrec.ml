(* apply.ml: safe but ill-typed *)
let apply f x = f x
let check (x:int) y = assert (x = y)
let main (n:int) = apply (check n) n
