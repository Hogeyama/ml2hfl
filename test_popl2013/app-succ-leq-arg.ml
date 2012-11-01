let succ x f = f (x + 1)
let k x f y = succ y f
let rec app x f = if Random.bool () then app x (k x f) else f x
let check x y = if x <= y then () else assert false
let main i = app i (check i)
