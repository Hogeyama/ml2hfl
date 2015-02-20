let add x y = x + y
let rec sum n =
  if n <= 0 then
    0
  else
    add n (sum (n-1))


(*{SPEC}
type sum : x:int -> {r:int | r >= x}
{SPEC}*)
