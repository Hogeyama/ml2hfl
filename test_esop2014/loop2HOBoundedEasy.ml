let id (u:unit) = u
let app (f:unit->unit->unit) (u:unit) = f u ()
let rec f m n (u:unit) =
if m > 0 then app (f (m-1) n)
else if n > 0 then app (f m (n-1))
else id
let main (u:unit) = 
  let n = Random.int 0 in
  let m = Random.int 0 in
  if n>0 && m>0 then f n m () () else ()
