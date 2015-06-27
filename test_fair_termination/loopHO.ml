(* CAV 2015 *)

let rec loop h n =
  if n > 0 then
    let d = Random.int 0 in
    let _ = if d < 0 then event "A" else () in
    let n_next = n + d in
    h n_next (loop h)
  else ()
let app n k = k n
let main r = loop app r

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
