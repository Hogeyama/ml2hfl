let rec mult n m =
  if n <= 0 || m <= 0 then
    0
  else
    n + mult n (m-1)

(*{SPEC}
type mult : n:int -> {m:int | n=m} -> {r:int | n < r}
{SPEC}*)
