(* apply-param.ml: verification succeeded *)
let apply a f x = f x
let check (x:int) y = assert (x = y)
let main (n:int) = apply (ExtFuncs.f1_1 n) (check n) n
