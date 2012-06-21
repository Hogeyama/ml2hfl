let rec app x f = if Random.int 0 = 0 then app (x + 1) f else f x
let check x y = if x <= y then () else assert false
let main i = app i (check i)
