(* CAV 2015 *)

let app (h:unit -> unit -> unit) (v:unit) = (h () v : unit)
let id (u:unit) = u
let rec g (x:int) (u:unit) =
  if x = 0 then id
  else if x > 0 then app (g (x-1))
  else let _ = event "A" in app (g (x-1))
let main r = g r () ()

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
