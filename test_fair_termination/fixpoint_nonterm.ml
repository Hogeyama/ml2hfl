(* CAV 2015 *)

let rec fix (f:int -> int) (n:int) =
  let n2 = f n in
  if n2 = n then n else fix f n2
let to_zero n =
  if n = 0 then 0
  else if n < 0 then
    let _ = event "A" in
    n - 1
  else n - 1
let main r =
  fix to_zero r

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
