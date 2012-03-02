(* a-max_mod.ml: verification failed *)
let make_array n i = n - i
let rec array_max i p (a:int->int) (n:int) m =
  if i >= n then
    m
  else
    let x = a i in
    let z = if x > m then x else m in
    array_max (i+1) (*(ExtFuncs.f4 i p n m)*)p a n z
let main n =
  if n > 0 then
    let m = array_max 0 (ExtFuncs.f1 n) (make_array n) n (-1) in
    assert (m >= n)
