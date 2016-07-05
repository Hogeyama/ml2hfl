(*{SPEC}

  fairness: (A, B)

  {SPEC}*)

let fail () = ()

let rec f g =
  let x = read_int () in
  if x < 0 then
    (event "A"; f g)
  else
    f g

let main () =
  f fail

(* Found Bug in 08d84ea, but fixed in next commit *)
