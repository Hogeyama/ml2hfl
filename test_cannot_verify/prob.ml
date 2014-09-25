(* from "Program verification as probabilistic inference" *)

let rec f x y =
  if x < 100 then
    if x < 50 then
      f (x + 1) y
    else
      f (x + 1) (y + 1)
  else
    y

let main =
 assert (not (f 0 50 = 100))
