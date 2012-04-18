(* verification succeeded *)
let rec array_max i (n:int) ex (ar:int->int) m =
  if i >= n then
    m
  else
    let x = ar i in
    let z = if x > m then x else m in
    array_max (i + 1) n ex ar z
let main n x =
  if n > 0 && x >= 0 then
    let m = array_max 0 n x (fun i -> x - i) (-1) in
    assert (m >= x)
