
let rec sum n =
  if n <= 0
  then 0
  else n + sum (n - 1)

let rec sumsum (n,m) =
  if n <= 0
  then 0, sum m
  else
    if m <= 0
    then sum n, 0
    else
      let r1,r2 = sumsum (n-1,m-1) in
      n+r1, m+r2

let main n =
  if n > 0
  then
    let r1,r2 = sumsum (n, n-1) in
    assert (r1 = n + r2)
