let make_array n i = n - i
let update a i x j = if i = j then x else a i
let rec array_max (n:int) i (a:int->int) m =
  if i >= n then
    m
  else
    let x = a i in
    let z = if x > m then x else m in
    array_max n (i+1) a z
let compare_array a1 a2 m = assert (array_max m 0 a1 (-1) <= array_max m 0 a2 (-1))
let main m n k =
  if m <= n && m < k then
		  let a1 = make_array k in
		  let a2 = update a1 m n in
		  compare_array a1 a2 k
