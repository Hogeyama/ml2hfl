(*
START: 0;

FROM: 0;
TO: 1;

FROM: 1;
  assume(x>1);
TO: 20;

FROM: 20;
   rho := nondet();
TO: 21;
         
FROM: 21;
  assume(rho>0);
TO: 3;

FROM: 3;
  y := y+1;
  assume(z>1);
  z := z-1;
TO: 20;
                    
FROM: 21;
  assume(rho <= 0);
  x := x-y;
  z := nondet();
TO: 1;
*)

let rec state1 rho x y z =
  if x>1 then state20 rho x y z else ()
and state20 rho x y z =
  let rho_next = read_int () in
  state21 rho_next x y z
and state21 rho x y z =
  if rho > 0 then
    state3 rho x y z
  else
    let z_next = read_int () in
    state1 rho (x-y) y z_next
and state3 rho x y z =
  if z>1 then
    state20 rho x (y+1) (z-1)
  else ()
let main () =
  let rho = read_int () in
  let x = read_int () in
  let y = read_int () in
  let z = read_int () in
  state1 rho x y z; assert false
