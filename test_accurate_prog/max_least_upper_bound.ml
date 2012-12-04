let max x y = if x > y then x else y
let main x y u = if x <= u && y <= u then assert (max x y <= u)
