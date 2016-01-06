(*{SPEC}

  fairness: (A, B)

  {SPEC}*)


let rec f () =
  let x = read_int () in
  if x < 0 then
    (event "B";
     ())
  else
    (event "A";
     f();f())

(* found *)
