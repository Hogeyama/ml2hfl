let lock st = assert (not st); true
let unlock st = assert st; false
let f n st = if n > 0 then lock(st) else st
let g n st = if n >= 0 then unlock(st) else st
let main n = assert (not (g n (f n false)))
