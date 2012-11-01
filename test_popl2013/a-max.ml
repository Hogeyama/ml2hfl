let rec array_max i n (*ex*) ar m =
  if i >= n then
    m
  else
    let x = ar i in
    let z = if x > m then x else m in
    array_max (i + 1) n (*0*) ar z
let rec rand_array x i = Random.int (x + 1)
let main n x =
  if n > 0 && x >= 0 then
    let m = array_max 0 n (*x*) (rand_array x) (-1) in
    assert (m <= x)
