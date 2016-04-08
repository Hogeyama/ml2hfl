(*{SPEC}

  fairness: (A, B)

  {SPEC}*)

let rec f x =
  let y = read_int () in
  if x < y then
    (event "B";
     f y)
  else
    (event "A";
     f x)
let main () =
  f (read_int ())

(* option: {-expand-ce-count 10} *)
(* found *)
