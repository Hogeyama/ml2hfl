(* apply-inst.ml: verification succeeded *)
let apply a f x = f x
let check (x:int) y = assert (x = y)
let main (n:int) = apply n (check n) n
