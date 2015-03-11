(*
START: 0;

FROM: 0;
x:=x+1;
TO: 1;

FROM: 1;
assume(x>=0);
x:=x+1;
TO: 1;
*)

let rec state1 x =
  if x >= 0 then
    state1 (x+1)
  else ()
let main () =
  let x = read_int () in
  state1 (x+1); assert false