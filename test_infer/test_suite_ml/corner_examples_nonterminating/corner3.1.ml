(*
START: 0;

FROM: 0;
TO: 1;

FROM: 0;
TO: 2;

FROM:1;
assume(x>0);
x := x-1;
TO:1;

FROM:2;
assume(x>0);
y := y-1;
TO:2;
*)

let rec state1 x y =
  if x>0 then state1 (x-1) y else ()
let rec state2 x y =
  if x>0 then state2 x (y-1) else ()
let main () =
  let x = read_int () in
  let y = read_int () in
  let branch = read_int () in
  (if branch > 0 then state1 x y else state2 x y); assert false