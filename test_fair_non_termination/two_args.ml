(*{SPEC}

  valcegar #randint_1:
  ((int -> X) -> X)

  valcegar #randint_2:
  (a:int -> (b:int[a < b; a >= b] -> X) -> X)

  valcegar f_2061:
  (a:int -> b:int[a < b; a >= b] -> (unit -> X) -> X)

  fairness: (A, B)

  {SPEC}*)


let rec f a b =
  if a < b then
    (event "A";
     f b a)
  else
    (event "B";
     f b a)
let main =
  let x = read_int () in
  let y = read_int () in
  f x y

(* found *)
