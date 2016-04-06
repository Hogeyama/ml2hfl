(*{SPEC}

  fairness: (A, B)

  {SPEC}*)

let ev_a () = event "A"; ()
let ev_b () = event "B"; ()

let rec f ev1 ev2 =
  ev1();
  f ev2 ev1

let main () =
  f ev_a ev_b
