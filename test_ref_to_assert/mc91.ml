let rec mc91 x =
  if x > 100 then
    x - 10
  else
    mc91 (mc91 (x + 11))

(*{SPEC}
type mc91 : {n:int | n <= 101} -> {r:int | r = 91}
{SPEC}*)
