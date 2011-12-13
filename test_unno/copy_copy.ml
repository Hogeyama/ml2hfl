let rec copy x = if x = 0 then 0 else 1 + copy (x - 1)
let check x y = assert (x = y)
let main n = check (copy (copy n)) n
