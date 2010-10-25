let f x g = g(x+1) in
let h z y = assert (y>z+1) in
  if n>=0 then f n (h n) else ()
