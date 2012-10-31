


let succ x = x in
let rec map n f =
  if n = 0
  then 0
  else f (map (n-1) f)
in
  assert (map n succ <= n)







