(*
START: 0;

FROM: 0;
TO: 1;

FROM: 1;
TO: 2;

FROM: 2;
TO: 2;
         
FROM: 2;
TO: 1;
*)

let rec state1 () = (state2 () : unit)
and state2 () =
  let br = read_int () in
  if br > 0 then state2 () else state1 ()
let main () = state1 (); assert false
