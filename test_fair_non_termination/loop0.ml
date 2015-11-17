(*{SPEC}

  valcegar #randint_1 :
  (unit -> (x:int[x > 0] -> X) -> X)

  valcegar f_loop :
  (unit -> (unit -> X) -> x:int[x > 0] -> X)

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
