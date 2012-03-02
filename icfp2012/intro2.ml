(* intro2.ml: verification succeeded *)
let rec app a f x = if Random.int 0 = 0 then app (ExtFuncs.f1 a) f (x + 1) else f x
let check x y = if x <= y then () else assert false
let main i = app (ExtFuncs.g1 i) (check i) i
