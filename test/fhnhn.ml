(*
USED: PLDI2011 as fhnhn
*)

let f x y = assert (not ((x () > 0) && (y () <= 0)))
let h x y = x
let rec g n = f (h n) (h n)
