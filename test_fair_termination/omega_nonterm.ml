let ign (x:int) = ()
let rec omega (x:int) =
  let _ = event "A" in
  (omega x:unit)
let f (x:int -> unit) (y:int -> unit) (z:int) =
  if z>0 then y z else x z
let main r = f (f omega ign) ign r

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
