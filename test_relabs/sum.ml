(*
let rec sum n =
  if n <= 0
  then 0
  else n + sum (n - 1)

let rec sum_acc n a =
  if n <= 0
  then a
  else sum (n - 1) (n + a)

let main n a =
  assert (a + sum n = sum_acc n a)
*)

let rec sum n =
  if n <= 0
  then 0
  else
    let s = sum (n - 1) in
    n + s

let rec sum_acc n a =
  if n <= 0
  then a
  else sum_acc (n - 1) (n + a)

let sum_sum_acc n a = sum n, sum_acc n a
(*
let rec sum_sum_acc n a =
  if n <= 0
  then 0, a
  else let s1,s2 = sum_sum_acc (n-1) (n+a) in
       n+s1, s2
*)

let main n a =
  let s1,s2 = sum_sum_acc n a in
  assert (a + s1 = s2)
