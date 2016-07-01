
let apply f (x:int) (k:(int->unit)) = f x k

let double x k = k (x+x)

let main n =
  if n >= 0 then
  let k0 r = assert (n <= r) in
  apply double n k0
