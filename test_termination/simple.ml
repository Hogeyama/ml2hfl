let rec f n = if n > 0 then f (n-1) else () in f (Random.int 0)
