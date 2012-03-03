(* simple1-param.ml: verification succeeded *)
let succ a f x = f (x + 1)
let rec app x a f = if Random.int 0 = 0 then app x (ExtFuncs.f1_2 x a) (succ (ExtFuncs.f1_2 x a) f) else f x
let check x y = if x <= y then () else assert false
let main i = app i (ExtFuncs.f1_1 i) (check i)
