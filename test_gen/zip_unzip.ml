let rec loop x = loop x
let f g x y = g (x + 1) (y + 1)
let rec unzip x k =
  if x=0 then k x x (*k 0 0‚È‚çŒŸØ‚É¬Œ÷*)
  else unzip (x - 1) (f k)
let rec zip x y =
  if x = 0 then if y = 0 then 0 else loop()
  else if y = 0 then fail () else 1 + zip (x - 1) (y - 1)
let main n = let x = unzip n zip in ()

