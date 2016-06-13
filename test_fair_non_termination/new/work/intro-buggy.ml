let rec f x =
  if x < 0 then ()
  else if x = 0 then (event "A"; ())
  (* else (f 0; f 1)  *)
  else (f x; f 1) (* modified *)

let main () =
  f (read_int ())

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
