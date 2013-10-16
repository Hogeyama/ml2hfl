let rec mult n m =
  if m = 0
  then 0
  else
    let r = mult n (m-1) in
    n + r

let main n m k =
  mult n (mult m k) = mult (mult n m) k
