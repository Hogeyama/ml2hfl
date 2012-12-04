let mad_max x y =
  if x = 42 && y = 42 then 43
  else if x > y then x else y
let main x = assert (mad_max x x = x)
