let rec append x y =
  if x = 0 then y else 1 + append (x - 1) y
let rec rev n =
  if n = 0 then 0 else append (rev (n - 1)) 1
let main n = assert (rev n >= n)
