(*
START: 0;

FROM: 0;
TO: 5;

FROM: 5;
  assume(x>=x);
TO: 3;
         
FROM: 3;
  assume(x>=x);
TO: 5;

FROM: 2;
TO: 2;
*)

let rec state5 (x:int) =
  if x>=x then state3 x else ()
and state3 (x:int) =
  if x>=x then state5 x else ()
let main () =
  let x = read_int () in
  state5 x; assert false
