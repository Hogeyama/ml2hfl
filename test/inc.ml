let rec ar i = 0 in
let update a i x j = if j=i then x else a j in
let rec g e a j =
 if j<e then
   (assert(0<=j && j<e);
    g e (update a j (a(j)+1)) (j+1))
 else ()
in
 g n ar 0

