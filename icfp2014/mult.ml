let rec mult n m =
  if m = 0
  then 0
  else
    let r = mult n (m-1) in
    n + r

let rec mult_acc n m a =
  if m = 0
  then a
  else mult_acc n (m-1) (n+a)

let mult_mult_acc n m a = mult n m, mult_acc n m a

let main n m a =
  let s1,s2 = mult_mult_acc n m a in
  assert (a + s1 = s2)
