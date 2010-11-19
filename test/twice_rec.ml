let twice f x = f(f x) in
let rec g x = if x <=0 then 1 else 2+g(x-1) in
let x = twice g n in
  assert (x>0)
