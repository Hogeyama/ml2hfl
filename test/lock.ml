




let lock st = assert (not st); true in
let unlock st = assert st; false in
let f n st = if n > 0 then lock(st) else st in
let g n st = if n > 0 then unlock(st) else st in
  assert (not (g n (f n false)))
