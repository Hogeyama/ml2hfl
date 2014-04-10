let rec mult (n, m) =
  if m = 0
  then 0
  else
    let r = mult (n, m-1) in
    n + r

let rec mult_acc (n, m, a) =
  if m = 0
  then a
  else mult_acc (n, m-1, n+a)

let mma = mult, mult_acc

let mult = fst mma
let mult_acc = snd mma

let main n m a =
  let s1 = mult (n, m) in
  let s2 = mult_acc (n, m, a) in
  assert (a + s1 = s2)
