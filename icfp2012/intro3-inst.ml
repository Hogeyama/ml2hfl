(* intro3-inst.ml: verification succeeded *)
let succ b f x = f (x + 1)
let rec app3 a f b g = if Random.int 0 = 0 then app3 a (succ a f) b g else g a f
let app x a f = f x
let check x y = if x <= y then () else assert false
let main i = app3 i (check i) i (app i)
