let f x g = g(x+1) in
let h y = assert (y>0) in
 if n>0 then f n h else ();

let f x g = g(x+1) in
let h y = assert (y>0) in
 if n>=0 then f n h else ();

let f x g = g(x+1) in
let h z y = assert (y>z) in
 if n>=0 then f n (h n) else ();

let rec sum n =
 if n <= 0
 then 0
 else n + sum (n-1)
in
 assert (n <= sum n);

let rec mult n m =
 if n <= 0 || m <= 0
 then 0
 else n + mult n (m-1)
in
 assert (n <= mult n n);

let max max2 x y z =
 max2 (max2 x y) z
in
let f x y =
 if x >= y
 then x
 else y
in
let m = max f x y z in
 assert (f x m <= m);

let rec m x =
 if x > 100
 then x - 10
 else m (m (x + 11))
in
 if n <= 101
 then assert (m n = 91)
 else ()
