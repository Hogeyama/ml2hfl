
let succ ex f x = f (x + 1)
let rec app x f = if Random.bool () then app (x - 1) (succ (ExtFuncs.f1_1 x) f) else f x
let check x y = if x = y then () else assert false
let main n = app n (check n)
