(*{SPEC}

  valcegar #randint_1 :
  (unit -> (x:int[x > 0; x <= 0] -> X) -> X)

  valcegar f_f :
  (unit -> (unit -> X) -> x:int[x > 0; x <= 0] -> X)

  fairness: (A, B)

  {SPEC}*)

let ev_a _ = event "A"; ()
let ev_b _ = event "B"; ()
let rec f ev =
  ev ();
  let x = read_int () in
<<<<<<< HEAD
  let y = read_int () in
  if x < y then
    if x > y + 1 then
      ()
    else
      f ev_b
  else
    f ev_a
let main () =
  f ev_a
=======
  if x > 0 then
    if x <= 0 then
      ()
    else
      f ()
  else
    f ()
>>>>>>> parent of fbee9e9... Merge remote-tracking branch 'origin/fair_non_termination'

(* option: {-expand-ce-count 10} *)
(* found *)
