let c q = ()
let b x q = x 1
let a x y q = if q=0 then (x 0; y 0) else assert false

let rec f n (x:int->int) (q:int) : int = if n <= 0 then x q else a x (f (n-1) (b x)) q

(*{SPEC}
type f : {n:int | n=0} -> (int -> int) -> int -> int
{SPEC}*)
