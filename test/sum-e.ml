(*
USED: PLDI2011 as sum-e
*)

let rec sum n =
  if n <= 0
  then 0
  else n + sum (n-1)
in
  assert (n + 1 <= sum n)
