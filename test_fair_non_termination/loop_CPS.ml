(*{SPEC}

  fairness: (A, B)

  {SPEC}*)

let ev_a (k:unit->unit) = event "A"; k ()
let ev_b (k:unit->unit) = event "B"; k ()
let rec cont () =
  let x = read_int () in
  if x > 0 then
    loop ev_a
  else
    loop ev_b
and loop ev =
  ev cont
let main = loop ev_a
