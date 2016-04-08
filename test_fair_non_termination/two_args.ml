(*{SPEC}

  valcegar #randint_1:
  ((int -> X) -> X)

  valcegar #randint_2:
  (a:int -> (b:int[a < b; a >= b] -> X) -> X)

  valcegar f_2061:
  (a:int -> b:int[a < b; a >= b] -> (unit -> X) -> X)

  fairness: (A, B)

  {SPEC}*)

let rec update_max x =
  let y = read_int () in
  if x < y then
    (event "B";
     update_max y)
  else
    (event "A";
     update_max x)
let main () =
  let x = read_int () in
  update_max x

(* option: {-expand-ce-count 10} *)
(* found *)
