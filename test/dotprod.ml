
let make_array n = n in
let arraysize src = src in
let update des i x = if 0 <= i && i < des then () else fail () in
let sub src i = (if 0 <= i && i < src then () else fail ()); 0 in

let rec dotprod_aux v1 v2 i n sum =
  if i = n
  then sum
  else dotprod_aux v1 v2 (i+1) n (sum + (sub v1 i) * (sub v2 i))
in
let dotprod v1 v2 = dotprod_aux v1 v2 0 (arraysize v1) 0 in

let v1 = make_array n in
let v2 = make_array m in
  if 0<=n && n=m then (dotprod v1 v2; ()) else ()

