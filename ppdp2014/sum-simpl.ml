
let rec sum n =
  if n <= 0
  then 0
  else n + sum (n - 1)

let main n =
  if n > 0
  then
    let r1 = sum n in
    let r2 = sum (n-1) in
    assert (r1 = n + r2)
