let id (x:int) = x in
let rec omega (x:int) = (omega x:int) in
let f (x:int -> int) (y:int -> int) (z:int) = y z in
  f (f id omega) id 1
