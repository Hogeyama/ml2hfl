let succ f x = f (x + 1)
let rec app (*ex*) f x = if Random.bool () then app (*(x - 1)*) (succ f) (x - 1) else f x
let check x y = if x = y then () else assert false
let main (u:unit) = app (*0*) (check 0) 0
