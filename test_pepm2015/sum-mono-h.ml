
let rec sum n =
  if n <= 0
  then 0
  else
    let s = sum (n - 1) in
    n + s

let rec sumsum n m =
  if n <= 0 then
    0, sum m
  else if m <= 0 then
    sum n, 0
  else
    let s1,s2 = sumsum (n-1) (m-1) in
    n+s1, m+s2

let main n m =
  if n < m
  then
    let r1,r2 = sumsum n m in
    assert (r1 <= r2)
