let f g x y = g (x+1) (y+1)
let rec unzip x k : unit =
 if x=0 then k 0 0
 else
   unzip (x-1) (f k)

let rec zip x y =
 if x=0 then
  if y=0 then 0
    else assert false
 else if y=0 then assert false
  else 1 + zip (x-1) (y-1)

(*{SPEC}
type unzip : x:int -> (y:int -> {z:int | y = z} -> unit) -> unit
type zip : y:int -> {z:int | y = z} -> int
{SPEC}*)
