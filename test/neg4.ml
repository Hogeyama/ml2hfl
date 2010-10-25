let g x y = x in
let twice f x y = f (f x) y in
let neg x y = - x () in
  if n>=0
  then
     let z = twice neg (g n) () in
       assert (z>=0)
  else ()
