let rec fsum f n =
  if n <= 0
  then 0
  else f n + fsum f (n-1)

(*{SPEC}
type fsum : ({x:int | x > 0} -> {r:int | r >= x}) -> y:int -> {s:int | s >= y}
{SPEC}*)
