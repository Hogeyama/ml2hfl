let rec f n =
  if n<0 then ()
  else
    let r = read_int () in
    if r>0 then
      (event "B"; (* Added *)
       f (n-1))
    else
      (event "A";
       f (n+1))
let main () =
  let r = read_int () in
  f r

(* old SPEC
  fairness: (A, Never)
{SPEC}*)

(*{SPEC}
  fairness: (A, B)
{SPEC}*)
