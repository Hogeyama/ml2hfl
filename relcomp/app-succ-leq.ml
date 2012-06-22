let succ f x = f (x + 1)
let rec app x f = if Random.bool () then app x (succ f) else f x
let check x y = if x <= y then () else assert false
let main i = app i (check i)
