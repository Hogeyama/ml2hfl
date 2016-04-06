(*{SPEC}

  fairness: (A, Never)

  {SPEC}*)


let rec f () =
  let x = read_int () in
  let y = read_int () in
  if x < y then
    f()
  else
    ()

(* found *)
