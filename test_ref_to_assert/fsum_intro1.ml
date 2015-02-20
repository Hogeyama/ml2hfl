let rec fsum f n =
  if n <= 0
  then 0
  else f n + fsum f (n-1)

let double x = x + x

let main n = fsum double n

(*{SPEC}
type main : x:int -> {r:int | r >= x}
{SPEC}*)
