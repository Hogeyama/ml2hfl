(*{SPEC}

  valcegar #randint_1 :
  (unit -> (int -> X) -> X)

  valcegar #randint_2 :
  (unit -> x:int -> (y:int[x < y; x >= y] -> X) -> X)

  valcegar f_f_2276:
  (unit -> x:int -> (unit -> X) -> y:int[x < y; x >= y] -> X)

  fairness: (A, Never)

  {SPEC}*)


let rec f () =
  let x = read_int () in
  let y = read_int () in
  if x < y then
    f()
  else
    ()

(* found *)
