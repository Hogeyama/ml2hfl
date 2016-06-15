(*{SPEC}
  fairness: (A, Never)
{SPEC}*)

(* option expand-ce-count 10*)
let rec repeat g =
  g (read_int ());
  repeat g

let rec f x =
  if x>0 then
  (* f (x-1) *)
    f x
  else
    (event "A";())

let main () =
  repeat f
