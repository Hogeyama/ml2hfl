let succ ex f x = f (x + 1)
let rec app x f = if Random.int 0 = 0 then app x (succ (ExtFuncs.f1_1 x) f) else f x
let check x y = if x <= y then () else assert false
let main i = app i (check i)
