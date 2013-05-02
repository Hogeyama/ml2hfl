let rec assume b = if b then () else assume b
let rec square x =
  if x = 0
  then 0
  else 2*x-1 + square (x-1)
let rec mult x y =
  if y = 0
  then 0
  else x + mult x (y-1)
let rec square_mult (x, y, z) =
  if x = 0
  then 0, mult y z
  else
    if z = 0
    then square x, 0
    else
      let r1,r2 = square_mult (x-1, y, z-1) in
        2*x-1 + r1, y + r2
let rec check (f,g,fg) =
  let x = Random.int 0 in
  let r1 = f x in
  let r1',r2 = fg (x,x,x) in
    assume (r1 = r1');
    assume (r1 = r2);
    if r1 = r2 then (assert (0=0); 0) else (assert (1=0); 1)
let main () = check (square, mult, square_mult)
