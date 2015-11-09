(*{SPEC}
  valcegar f_f:
  (x:int[x>0; x<=0] -> (unit -> X) -> unit -> X)

  valcegar f_f:
  (x:int[x>0; x <= 0] -> (unit -> X) -> X)

  fairness: (A, Never)

  {SPEC}*)

let rec f n =
  if n > 0 then
    f n
  else
    (event "A";
     f (n+1))

let main () = f (read_int ())

(* TODO *)
