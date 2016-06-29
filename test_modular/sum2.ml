(* {SPEC}
type main : int -> unit
{SPEC}*)



let rec sum n =
  if n <= 0
  then 0
  else n + sum (n-1)

let sum2 n = sum n

let main n =
  assert (sum2 n >= n)
