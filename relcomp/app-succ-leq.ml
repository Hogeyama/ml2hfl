(* app-succ-leq.ml: verification succeeded *)
let succ f x = f (x + 1)
let rec app x f = if Random.int 0 = 0 then app x (succ f) else f x
let check x y = if x <= y then () else assert false
let main i = app i (check i)
