let add x1 x2 = x1 + x2
let rec repeat ex f k x = if k <= 0 then x else f (repeat ex f (k - 1) x)
let main n k = if n >= 0 && k > 0 then assert (repeat ex (add n) k 0 >= n)
