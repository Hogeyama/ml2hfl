(* applyrec-inst.ml: verification succeeded *)
let apply ex f x = f x
let check (x:int) y = assert (x = y)
let rec loop n = apply n (check n) n; (loop (n + 1) : int)
let main (x:int) = loop x (* if 0 is passed instead of x, then MoCHi does not terminate *)
