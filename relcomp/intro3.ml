let succ (*ex*) f x = f (x + 1)
let rec app3 (*ex1*) f (*ex2*) g = if Random.bool () then app3 (*ex1*) (succ (*ex1*) f) (*ex2*) g else g (*ex2*) f
let app x (*ex*) f = f x
let check x y = if x <= y then () else assert false
let main i = app3 (*i*) (check i) (*i*) (app i)
