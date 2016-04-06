(*{SPEC}

  fairness: (A, B)

  {SPEC}*)

let ev_a () = event "A"; ()
let ev_b () = event "B"; ()

let rec f ev =
  ev ();
  let p = read_int () in
  if p < 0 then
    f ev_a
  else
    f ev_b

let main () =
  f ev_a
