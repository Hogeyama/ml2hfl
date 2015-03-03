let id (x:int) = x in
let rec omega (x:int) = (omega x:int) in
let f (x:int -> int) (y:int -> int) (z:int) = y z in
let rec app (h:int -> int) (x:int) = h x in
  f (app (f (app id) (app omega))) (app id) 1
