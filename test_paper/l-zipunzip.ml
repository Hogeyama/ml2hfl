let f g x y = g (x + 1) (y + 1) in
let rec unzip x k =
 if x=0 then k 0 0
 else
   unzip (x - 1) (f k)
in
let rec zip x y =
 if x = 0 then
  if y = 0 then 0
    else fail ()
 else if y = 0 then fail ()
  else 1 + zip (x - 1) (y - 1)
in
 let x = unzip n zip in ()
