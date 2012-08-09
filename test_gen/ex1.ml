let f b = assert b
let g x = x + 1
let main m = f (m < g m)
