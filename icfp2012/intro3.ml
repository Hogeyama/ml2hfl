(* intro3.ml: verification succeeded *)
let succ a f x = f (x + 1)
let rec app x a f = if Random.int 0 = 0 then app x (ExtFuncs.f2 x a) (succ (ExtFuncs.f1 x) f) else f x
let check x y = if x <= y then () else assert false
let main i = app i (ExtFuncs.g1 i) (check i)
