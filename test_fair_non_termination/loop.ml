(*{SPEC}

  valcegar #randint_1 :
  (unit -> (x:int[x > 0; x <= 0] -> X) -> X)

  valcegar f_loop :
  (unit -> (unit -> X) -> x:int[x > 0; x <= 0] -> X)

  fairness: (A, B)

  {SPEC}*)

let app_unit f = f ()
let rec loop () =
  let x = read_int () in
  if x > 0 then
    (event "A";
     app_unit loop)
  else
    (event "B";
     app_unit loop)
let main () = loop ()

(* option: {-expand-ce-count 10} *)
(* found *)
