(* succ‚Ìˆø”‚Ì‡”Ô‚ğ•Ï‚¦‚é‚Ì‚Í’Pƒ‚È•À‚Ñ‘Ö‚¦‚Å‚Í–³— *)
let succ f x = f (x + 1)
let rec app x f = if Random.int 0 = 0 then app (x - 1) (succ f) else f x
let check x y = if x = y then () else assert false
let main n = app n (check n)
