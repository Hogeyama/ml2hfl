let rec gib f n =
  if n<2 then f n else
    ((gib f) (n-1)) + ((gib f) (n-2))
in
let f x = x in
  if ((gib f) 10) < 50
  then fail ()
  else ()
