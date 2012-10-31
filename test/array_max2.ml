let k m n a =
 if 0<=j && j<n then assert(a(j) <= m) else () in
let rec max m i n a =
 if i>=n then k m n a
 else
   let y = a(i) in
     if y>m then max y (i+1) n a
     else max m (i+1) n a
in
let mk_array n a i = 1
in
 if i>=0 && i<=0 then max z i n (mk_array n a) else ()
