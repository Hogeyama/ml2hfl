let make_array n i = i
let check m n a j = if j>=0 && j<n then assert(a(j)<=m) else ()
let inc = 1
let rec array_max m n i a j =
  if i >= n then
    check m n a j
  else
    let x = a i in
    let z = if x>m then x else m in
      if inc>0 && inc<=1 then
        array_max z n (i+inc) a j
let main i j n =
    if i>=0 && i<1 && j>=0 && j<n then
      array_max 0 n i (make_array n) j
