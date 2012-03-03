let make_array n i = 0
let update c1 c2 ar i x j = if i = j then x else ar j
let checksum c1 c2 ar x = assert ((ar 0) + (ar 1) = x)
let main a b = 
  let m = update a b (update a 0 (make_array 2) 0 a) 1 b in
  checksum a b m (a + b)
