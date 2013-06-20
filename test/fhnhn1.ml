let f x y = assert (not ((x () > 0) && (y () <= 0)))
let h x y = x
let next x = if x>0 then -x else -x+1
let rec g n = f (h n) (h n); g (next n)
