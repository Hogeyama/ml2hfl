(* CAV 2015 *)

let app f (x:int) (u:unit) = (f x u: unit)
let id (u:unit) = u
let rec g (x:int) =
  if x = 0 then id
  else if x > 0 then app g (x-1)
  else let _ = event "A" in app g (x-1)
let main t = g t ()

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
