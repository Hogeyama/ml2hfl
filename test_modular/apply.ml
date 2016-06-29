
let apply f (x:int)  = f x

let double x  =  (x+x)

let main n =
  if n >= 0 then

  assert (n <= apply double n)
