(* @require closure handling
   @from Binary Reachability Analysis of Higher Order Functional Programs
         Ruslan Ledesma-Garza and Andrey Rybalchenko
         SAS 2012 *)

let rec f x = if x > 0 then f (x - 1) else lambda
and lambda x = x + 1 
let g = f 1
let main (u:unit) = g 2
