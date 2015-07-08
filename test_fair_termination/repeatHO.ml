let const x () = x
let pred g () = g () - 1
let rec repeat g = let x = Random.int 0 in g (const x); repeat g
let rec f g = if g ()>0 then f (pred g) else event "A"
let main = repeat f

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
