let apply f x = f x in
let g y z = assert (y=z) in
let rec k n = apply (g n) n; k(n+1) in
  k 0
