let f x g = g(x+1) in
let h y = assert (y>1) in
  if n>=0 then f n h else ()
