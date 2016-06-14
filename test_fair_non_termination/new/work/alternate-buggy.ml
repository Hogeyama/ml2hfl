(* CAV 2015 *)

let rec f g h z =
  let x = read_int () in
  if x>0 then
    g (f h g)
  else
    h (f h g)

(* let proceed u = let _ = event "A" in u () *)
let proceed u = let _ = event "B" in u ()
let halt u = ()
let main () = f proceed halt ()

(*{SPEC}
  fairness: (A, B)
{SPEC}*)
(*
  - change event: A -> B
  - change fainess: (A, Never) -> (A, B)
*)