(*{SPEC}

  fairness: (A, B)

  {SPEC}*)

let rec f () =
  let x = read_int () in
  let y = read_int () in
  if x < y then
    if x >= y then
      ()
    else
      (event "B";
       f ())
  else
    (event "A";
     f ())

(* found *)
