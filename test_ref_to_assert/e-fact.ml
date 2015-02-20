let rec fact n exn =
  if n <= 0 then
    exn 0
  else
    let exn n = if n = 0 then 1 else exn n in
    n * fact (n - 1) exn

(*{SPEC}
type fact : {n:int | n > 0} -> ({u:int | false} -> int) -> int
{SPEC}*)
