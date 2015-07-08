let const x () = x
let pred x g () = g () - 1
let rec repeat g = let x = Random.int 0 in g x (const x); repeat g
let rec f x g = if g ()>0 then f (x-1) (fun () -> g () - 1) else event "A"
let main = repeat f

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
