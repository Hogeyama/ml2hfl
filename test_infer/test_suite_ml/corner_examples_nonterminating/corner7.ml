(*
START: 0;

FROM: 0;
assume(y>=0);
y:=y+1;
x:=y;
TO: 1;

FROM: 1;
assume(x>=0);
x:=x-1;
TO: 1;

FROM:1;
assume(x<0);
TO:0;
*)

let rec state0 x y =
  if y >= 0 then
    let y_next = y + 1 in
    state1 y y_next
  else ()
and state1 x y =
  if x >= 0 then
    state1 (x-1) y
  else
    state0 x y
let main () =
  let x = read_int () in
  let y = read_int () in
  state0 x y; assert false
