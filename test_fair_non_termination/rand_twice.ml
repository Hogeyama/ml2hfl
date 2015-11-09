(*{SPEC}

  valcegar #randint_1 :
  (unit -> (x:int[x > 0; x <= 0] -> X) -> X)

  valcegar #randint_2 :
  (unit -> x:int -> (y:int[x < y; x >= y] -> X) -> X)

  fairness: (A, Never)

  {SPEC}*)


let rec f () =
  let x = read_int () in
  let y = read_int () in
  if x < y then
    f()
  else
    ()

(* TODO *)
