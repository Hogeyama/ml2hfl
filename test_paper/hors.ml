let c q = () in
let b x q = x 1 in
let a x y q = if q=0 then (x 0; y 0) else fail() in
let rec f n x q = if n <= 0 then x q else a x (f (n-1) (b x)) q
in
let s q = f n c q in
  s 0

