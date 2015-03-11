(*
START: 0;

FROM: 0;
TO: 1;

FROM: 1;
TO: 3;
         
FROM: 3;
TO: 1;
*)

let rec state1 () = (state3 (): unit)
and state3 () = state1 ()
let main () = state1 (); assert false
