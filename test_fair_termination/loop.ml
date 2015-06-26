let rec loop x = event "A"; loop x
let main = loop ()

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
