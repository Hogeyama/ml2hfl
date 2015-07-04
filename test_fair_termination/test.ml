let rec repeat g = g (Random.int 0); repeat g
let rec f x = if x>0 then f (x-1) else event "p"
let main = repeat f
