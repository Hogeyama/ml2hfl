let apply (*ex*) f x = f x
let check x y = assert (x = y)
let rec loop n : unit = apply (*n*) (check n) n; loop (n + 1)
let main (x:int) = loop x
(* if 0 is passed instead of x, then MoCHi does not terminate *)
