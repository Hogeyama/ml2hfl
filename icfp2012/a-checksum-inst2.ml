let make_array n i = 0
let update c ar i x j = if i = j then x else ar j
let checksum c ar x = assert ((ar 0) + (ar 1) = x)
let main a b = 
  checksum (a + b) (update (a + b) (update a (make_array 2) 0 a) 1 b) (a + b)
