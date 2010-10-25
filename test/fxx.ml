let f x y = if (x>0 && y<=0) then fail () else () in
let g x = f x x in 
  g n
