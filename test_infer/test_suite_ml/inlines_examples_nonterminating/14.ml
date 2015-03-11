(*
START: 0;

FROM: 0;
TO: 1;

FROM: 1;
  assume(0<=0);
TO: 3;
         
FROM: 3;
TO: 4;

FROM: 4;
TO: 1;
*)

let rec state1 () = if 0<=0 then state3 () else ()
and state3 () = state4 ()
and state4 () = state1 ()
let main () = state1 (); assert false
