
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

let main n a =
  let s1 = sum n in
  let s2 = sum_acc (n, a) in
  assert (a + s1 = s2)
