(*
let rec add x1 x2 =
   if x1 = 0 then
			  x2
   else
			  1 + (add (x1 - 1) x2)
*)
let add x1 x2 = x1 + x2
let comp f g x = f (g x)
let main n = assert (comp (add n) (add n) 0 >= 2 * n)
