let g y = if y () <= 0 then fail () else () in
let f x y = if (x () > 0) then g y else () in
let h x y = x in
let rec g n = f (h n) (h n) in
   g m
