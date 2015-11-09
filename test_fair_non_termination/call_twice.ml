(*{SPEC}

  valcegar #randint_1 :
  (unit -> (x:int[x > 0; x <= 0] -> X) -> X)

  valcegar f_f :
  (unit -> (unit -> X) -> x:int[x > 0; x <= 0] -> X)

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

(* TODO *)
