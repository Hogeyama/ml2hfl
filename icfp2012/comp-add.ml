let rec add n x =
   if n<=0 then x
   else 1 + (add (n-1) x)
let comp ex1 f ex2 g x = f (g x)
let main n = assert(comp n (add n) n (add n) 0 >= 2*n)
