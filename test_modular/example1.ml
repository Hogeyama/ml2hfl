let f x = if x <= 0 then x else 1
let main y = if y = 0 then assert (f y = y) (*else if y = 1 then assert (f y >= 0)*)
