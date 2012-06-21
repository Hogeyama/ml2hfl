let succ b f x = f (x + 1)
let rec app3 a f b g = if Random.int 0 = 0 then app3 (ExtFuncs.f1_2 a b) (succ (ExtFuncs.f2_2 a b) f) (ExtFuncs.f3_2 a b) g else g (ExtFuncs.f4_2 a b) f
let app x a f = f x
let check x y = if x <= y then () else assert false
let main i = app3 (ExtFuncs.f1_1 i) (check i) (ExtFuncs.f2_1 i) (app i)
