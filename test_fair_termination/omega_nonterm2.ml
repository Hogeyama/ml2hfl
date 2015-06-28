let ign (x:int) = ()
let rec omega (x:int) =
  let _ = event "A" in
  (omega x:unit)
let f (x:int -> unit) (y:int -> unit) (z:int) =
  let delta = Random.int 0 in
  if z>0 then y delta else x delta
let main r = f (f ign omega) ign r

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
