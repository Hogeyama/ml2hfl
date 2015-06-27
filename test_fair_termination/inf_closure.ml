(* CAV 2015 *)

let is_zero n = if n > 0 then let _ = event "A" in n = 0 else n = 0
let succ_app (f:int->bool) n = f (n + 1)
let rec f n (cond:int->bool) =
  if cond n then
    ()
  else
    f n (succ_app cond)
let main r = f r is_zero

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
