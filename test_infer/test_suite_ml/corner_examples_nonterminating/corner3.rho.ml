(*
START: 0;

FROM: 0;
TO: 1;

FROM: 1;
rho := nondet();
TO: 2;

FROM:2;
assume(rho>0);
assume(x>0);
x := x-1;
TO:1;

FROM:2;
assume(rho<=0);
assume(x>0);
y := y-1;
TO:1;
*)

let rec state1 rho x y =
  let rho_next = read_int () in
  state2 rho_next x y
and state2 rho x y =
  if rho > 0 then
    if x>0 then state1 rho (x-1) y else ()
  else
    if x>0 then state1 rho x (y-1) else ()
let main () =
  let rho = read_int () in
  let x = read_int () in
  let y = read_int () in
  state1 rho x y; assert false