(* a-max_mod.ml: verification failed *)
let make_array n i = n - i
let rec array_max (n:int) (a:int->int) i m =
  if i >= n then
    m
  else
    let x = a i in
    let z = if x>m then x else m in
    array_max n a (i+1) z
let main n i =
  if n>0 && i>=0 && i<=0 then
    let m = array_max n (make_array n) i (-1) in
    assert (m >= n)
