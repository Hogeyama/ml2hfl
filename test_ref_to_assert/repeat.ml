let rec repeat f n (s:int) =
  if n = 0 then
    s
  else
    f (repeat f (n-1) s)

(*{SPEC}
type repeat : (x:int -> {r:int | r > x}) -> n:int -> {s:int | s = 0} -> {r:int | r >= n}
{SPEC}*)
