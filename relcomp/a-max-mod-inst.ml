let rec array_max i ex (a:int->int) (n:int) m =
  if i >= n then
    m
  else
    let x = ar i in
    let z = if x > m then x else m in
    array_max (i + 1) ex ar n z
let main n =
  if n > 0 then
    let m = array_max 0 n (fun i -> n - i) n (-1) in
    assert (m >= n)
