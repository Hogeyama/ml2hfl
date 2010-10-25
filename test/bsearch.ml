
let make_array n = n in
let arraysize src = src in
let update des i x = if 0 <= i && i < des then () else fail () in
let sub src i = (if 0 <= i && i < src then () else fail ()); 0 in

let rec bs_aux key vec l u =
  if u < l
  then -1
  else
    let m = l + (u-l) / 2 in
    let x = sub vec m in
      if x < key then bs_aux key vec (m+1) u
      else if x > key then bs_aux key vec l (m-1)
             else m
in
let bsearch key vec = bs_aux key vec 0 (arraysize vec - 1) in

let v1 = make_array n in
let v2 = make_array m in
  if 0<=n && n=m then (bsearch v1 v2; ()) else ()

