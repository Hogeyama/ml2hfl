(*{SPEC}

  fairness: (A, B)

  {SPEC}*)

let rec loop () =
  let x = read_int () in
  if x > 0 then
    (event "A";
     loop ())
  else
    (event "B";
     loop ())

(* found *)
