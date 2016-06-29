
let rec zero m =
  if m <= 0 then
    0
  else
    zero (m-1)
let main n = assert (0 <= zero n)
