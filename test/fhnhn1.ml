let f x y = assert (not ((x () > 0) && (y () <= 0))) in
let h x y = x in
let next x = if x>0 then -x else -x+1 in
let rec g n = f (h n) (h n); g (next n) in
   g m
