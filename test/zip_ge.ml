let rec zip x y =
  if x = 0
  then
    if y = 0
    then 0
    else fail ()
  else
    if y = 0
    then fail ()
    else 1 + zip (x-1) (y-1)
in
  assert (zip n n >= n)
