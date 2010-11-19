let rec ar i = 0 in
let update a i x j = if j=i then x else a j in
let rec g a j e =
  if j < e
  then
    (assert(0 <= j); assert(j < e);
     g (update a j (a(j)+1)) (j+1) e)
  else ()
in
   g ar 0 n
