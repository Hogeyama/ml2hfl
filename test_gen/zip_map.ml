let rec zip x = if x=0 then 0 else 1 + zip (x-1)
let rec map x = if x=0 then 0 else 1 + map (x-1)
let main n =
  assert(map (zip n)>=n)
