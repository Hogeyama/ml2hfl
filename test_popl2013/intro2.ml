let rec app (*ex*) f x = if Random.bool () then app (*ex*) f (x + 1) else f x
let check x y = if x <= y then () else assert false
let main i = app (*i*) (check i) i
