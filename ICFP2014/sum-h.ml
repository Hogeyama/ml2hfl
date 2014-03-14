
let rec assume b = if b then () else assume b

let rec sum n =
  if n <= 0
  then 0
  else
    let s = sum (n - 1) in
    n + s

let rec sum_acc (n, a) =
  if n <= 0
  then a
  else sum_acc (n - 1, n + a)

let rec sum_sum_acc' (n,a) =
  if n <= 0
  then 0, sum_acc (n,a)
  else
    if n <= 0
    then sum n, a
    else
      let r1,r2 = sum_sum_acc' (n-1,n+a) in
      n + r1, r2

let rec sum_sum_acc (n, na) =
  if fst n = 0 then ((0,0), if fst n = 0 then (0,0) else (1,sum_acc (snd na)))
  else if fst na = 0 then ((1,sum (snd n)),(0,0)) else
    let r1,r2 = sum_sum_acc' (snd na) in
    ((1,r1),(1,r2))

let main n a =
  let s1,s2 = sum_sum_acc ((1,n), (1,(n,a))) in
  assert (a + snd s1 = snd s2)
