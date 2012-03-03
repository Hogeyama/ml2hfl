let make_array n i = 0
let update ar i x j = if i = j then x else ar j
let checksum ar x = assert ((ar 0) + (ar 1) = x)
let main a b = 
  let m = update (update (make_array 2) 0 a) 1 b in
  checksum m (a + b)
