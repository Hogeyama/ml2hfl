let rec dotprod n v1 v2 i sum =
  if i >= n then
    sum
  else
    dotprod n v1 v2 (i + 1) (sum + v1 i * v2 i)

(*{SPEC}
type dotprod : n:int -> ({i:int | 0 <= i && i < n} -> int) -> ({i:int | 0 <= i && i < n} -> int) -> {i:int | 0 <= i && i <= n} -> int -> int
{SPEC}*)
