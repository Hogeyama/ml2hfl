(*{SPEC}

  fairness: (A, B)

  {SPEC}*)

let ev_a () = event "A"; ()
let ev_b () = event "B"; ()


let rec f ev =
  ev ();
  let x = read_int () in
  if x < 0 then
    (f ev_b)
  else
    (f ev_a)

let main () =
  f ev_a
