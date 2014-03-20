
let rec assume b = if b then () else assume b

let rec mult (n, m) =
  if m = 0
  then 0
  else
    let s = mult (n, m-1) in
    n + s

let rec mult_acc (n, m, a) =
  if m = 0
  then a
  else mult_acc (n, m-1, n+a)

let rec mult_mult_acc' (n,m,a) =
  if m = 0
  then 0, mult_acc (n,m,a)
  else
    if m = 0
    then mult (n,m), a
    else
      let r1,r2 = mult_mult_acc' (n,m-1,n+a) in
      n + r1, r2

let rec mult_mult_acc (n, na) =
  if fst n = 0 then ((0,0), if fst n = 0 then (0,0) else (1,mult_acc (snd na)))
  else if fst na = 0 then ((1,mult (snd n)),(0,0)) else
    let r1,r2 = mult_mult_acc' (snd na) in
    ((1,r1),(1,r2))

let main n m a =
  let s1,s2 = mult_mult_acc ((1,(n,m)), (1,(n,m,a))) in
  assert (a + snd s1 = snd s2)
