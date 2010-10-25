




let lock st = assert (st=0); 1 in
let unlock st = assert (st=1); 0 in
let f n st = if n > 0 then lock(st) else st in
let g n st = if n > 0 then unlock(st) else st in
  assert (g n (f n 0) = 0)
