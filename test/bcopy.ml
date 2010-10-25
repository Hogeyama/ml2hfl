
let make_array n = n in
let arraysize src = src in
let update des i x = if 0 <= i && i < des then () else fail () in
let sub src i = (if 0 <= i && i < src then () else fail ()); 0 in

let rec bcopy_aux src des i m =
  if i = m
  then ()
  else
    begin
      update des i (sub src i);
      bcopy_aux src des (i+1) m
    end
in
let bcopy src des = bcopy_aux src des 0 (arraysize src) in
let array1 = make_array n in
let array2 = make_array m in
  if 0<=n && 0<=m && n<=m then bcopy array1 array2 else ()

