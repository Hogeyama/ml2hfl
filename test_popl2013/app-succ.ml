let succ (*ex*) f x = f (x + 1)
let rec app (*ex*) f x = if Random.bool () then app (* x is ok but ex is NG *) (succ (*x*) f) (x - 1) else f x
let check x y = if x = y then () else assert false
let main n = app (*n*) (check n) n
