let ign (x:int) = ()
let rec omega (x:int) =
  (event "A";
   omega x:unit)

let f (x:int -> unit) (y:int -> unit) (z:int) =
  let delta = read_int () in
  if z>0 then y delta else x delta

let main () =
  let r = read_int () in
  (*f (f ign omega) ign r*)
  f (f omega omega) omega r

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
