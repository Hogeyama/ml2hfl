(* {SPEC}
type main : int -> unit
{SPEC}*)



let rec sum n =
  if n <= 0
  then 0
  else n + sum (n-1)

let main n =
  assert (sum n >= n)
