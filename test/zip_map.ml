let rec loop x = loop x in
let rec zip x y =
 if x=0 then
   if y=0 then 0
     else loop()
 else if y=0 then loop()
   else 1 + zip (x-1) (y-1)
in
let rec map x =
  if x=0 then 0 else 1+ map (x-1)
in
 assert(map (zip n n)>=n)

