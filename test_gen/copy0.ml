let rec copy x = if x = 0 then 0 else 1 + copy (x - 1)
let main n = if n = 0 then assert (copy 0 >= n)
(* the following is OK 
let rec copy x = if x = 0 then x else 1 + copy (x - 1)
let main n = if n = 0 then assert (copy n >= n)
*)