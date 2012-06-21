let rec loop u = loop u
let rec array_max i (n:int) (ar:int->int) m =
  if i >= n then
    m
  else
    let x = ar i in
    let z = if x > m then x else m in
    array_max (i + 1) n ar z
let main n x =
  if n > 0 && x >= 0 then
    let m = array_max 0 n (fun i -> let n = Random.int 0 in if n <= x then n else loop ()) (-1) in
    assert (m <= x)
