let make_array n i = assert(0<=i && i<n); 0 in
let update i n des x = des i; let a j = if i=j then x else des i in a in
let rec inc3 m a i =
 if i>=m
 then ()
 else
   let a = update i m a ((a i)+1) in
     inc3 m a (i+1)
in
 if i=0 && n>0 then inc3 n (make_array n) i else ()
