let rec zip x y =
 if x = 0 then
   if y = 0 then x
   else fail ()
 else if y = 0 then fail ()
 else 1 + zip (x - 1) (y - 1)
in
let rec map x =
  if x = 0 then x else 1 + map (x - 1)
in
  assert (map (zip n n) = n)
