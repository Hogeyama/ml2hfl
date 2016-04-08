(*{SPEC}

  valcegar #randint_1 :
  (unit -> (x:int[x < 0; x >= 0] -> X) -> X)

  valcegar f_f :
  (unit -> (unit -> X) -> x:int[x < 0; x >= 0] -> X)

  fairness: (A, B)

  {SPEC}*)

let call_twice f = f (); f ()
let rec f () =
  let x = read_int () in
  if x < 0 then
    (event "B";
     ())
  else
    (event "A";
     call_twice f)
let main () = f ()
(* found *)
