(* a-max-orig.ml: verification failed *)
let make_array n i = n - i
let rec array_max n i (a:int->int) m =
  if i < 0 then
    m
  else
    let x = a i in
    let z = if x > m then x else m in
    array_max n (i - 1) a z
let main n =
  if n > 0 then
    let m = array_max n (n - 1) (make_array n) (-1) in
    assert (m >= n)
