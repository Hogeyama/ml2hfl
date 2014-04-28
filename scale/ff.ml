
let rec f x =
  if x <= 0
  then 0
  else 1 + f (x-1)

let rec ff x y =
  if x <= 0 then
    0, f y
  else if y <= 0 then
    f x, 0
  else
    let r1,r2 = ff (x-1) (y-1) in
    1+r1, 1+r2

let rec id x =
  if x <= 0
  then x
  else 1 + id (x-1)

let main n =
  if n < 0
  then ()
  else
    let n' = id n in
    let r1,r2 = ff 0 n' in
    let r1',r2' = ff n' n' in
    assert (r1' = r2')
