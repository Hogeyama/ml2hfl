let rec f x = if x < 0 then () else (event "q"; f (x-1))
let main : unit = f (Random.int 0)
