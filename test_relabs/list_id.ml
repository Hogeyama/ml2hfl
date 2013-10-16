let none = (true, 0)
let some x = (false, x)

let rec id xs =
  let (b,x) = xs 0 in
  if b
  then fun i -> none,none
  else
    let xs' i = xs (i + 1) in
    let yszs = id xs' in
    fun i -> if i = 0 then (some x, some x) else yszs (i-1)

let rec eq_list xsys =
  let (b1,x),(b2,y) = xsys 0 in
  if b1
  then
    if b2
    then true
    else false
  else
    if b2
    then false
    else
      let xs'ys' i = xsys (i+1) in
      x = y && eq_list xs'ys'

let rec make_list n =
  if n <= 0
  then fun i -> none
  else
    let x = Random.int 10 in
    let xs = make_list (n-1) in
    fun i -> if i=0 then some x else xs (i-1)

let main n =
  let xs = make_list n in
  let xsys = id xs in
  assert (eq_list xsys)
