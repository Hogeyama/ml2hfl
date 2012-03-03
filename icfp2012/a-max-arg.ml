(* a-max-arg.ml: safe but ill-typed *)
let make_array n i = n - i
let rec array_max i (a:int->int) (n:int) m =
  if i >= n then
    m
  else
    let x = a i in
    let z = if x > m then x else m in
    array_max (i+1) a n z
let main n =
  if n > 0 then
    let m = array_max 0 (make_array n) n (-1) in
    assert (m >= n)
