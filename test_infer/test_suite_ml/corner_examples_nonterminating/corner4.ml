(*
START: 0;

FROM: 0;
TO: 1;

FROM:1;
assume(w>0);
assume(x>0);
x := x-1;
TO:1;

FROM:1;
assume(w<=0);
assume(x>0);
y := y-1;
TO:1;
*)

let rec state1 w x y =
  if w>0 then
    if x>0 then state1 w (x-1) y else ()
  else
    if x>0 then state1 w x (y-1) else ()
let main () =
  let w = read_int () in
  let x = read_int () in
  let y = read_int () in
  state1 w x y; assert false