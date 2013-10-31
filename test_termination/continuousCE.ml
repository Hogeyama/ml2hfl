let isPositive n = n > 0
let decApp (g_exparam:int) (g:int -> bool) n = g (n-1)
let rec f n g_exparam g = if g n then f n (g_exparam+1) (decApp g_exparam g) else ()
let main (n:int) = f n 0 isPositive
