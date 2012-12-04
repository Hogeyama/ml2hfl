let max x y = if x > y then x else y
let main () = assert (max 1 5 = 5 && max 1 1 = 1 && max 3 2 = 3 && max 3 (-1) = 3 && max 1 (-3) = 1)
