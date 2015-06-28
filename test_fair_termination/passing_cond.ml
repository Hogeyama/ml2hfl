(* CAV 2015 *)

let isPositive (n:int) = n > 0
let isNegative (n:int) = let _ = event "A" in n < 0
let rec f (cond:int->bool) (x:int) =
  if cond x then f cond (x - 1) else ()
let main r n =
  if r > 0 then f isPositive n
           else f isNegative n

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
