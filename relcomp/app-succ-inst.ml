let succ ex f x = f (x + 1)
let rec app ex f x = if Random.int 0 = 0 then app x (succ x f) (x - 1) else f x
let check x y = if x = y then () else assert false
let main n = app n (check n) n
