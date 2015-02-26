(*
START: 0;

FROM: 0;
TO: 1;

FROM: 1;
  assume(x == y);
TO: 3;
         
FROM: 3;
  x := x+0;
TO: 1;
*)

let rec state1 x y = if x=y then state3 x y else ()
and state3 x y = state1 (x+0) y
let main () = 
  let x = read_int () in
  let y = read_int () in
  state1 x y; assert false
