
let make_array n = n in
let arraysize src = src in
let update des i x = assert (0 <= i && i < des) in
let sub src i = assert (0 <= i && i < src); 0 in

let rec dotprod_aux n v1 v2 i sum =
  if i = n
  then sum
  else dotprod_aux n v1 v2 (i+1) (sum + (sub v1 i) * (sub v2 i))
in
let dotprod v1 v2 = dotprod_aux (arraysize v1) v1 v2 0 0 in

let v1 = make_array n in
let v2 = make_array m in
  if 0<=n && n=m then (dotprod v1 v2; ()) else ()

