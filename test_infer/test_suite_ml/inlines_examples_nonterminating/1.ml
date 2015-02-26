(*
START: 0;

FROM: 0;
  x := 0;
  y := 0;
TO: 10;

FROM: 10;
   rho := nondet();
TO: 11;
         
FROM: 11;
  assume(rho>0);
  x := x+1;
  y := y+1;
TO: 10;
                    
FROM: 11;
  assume(rho <= 0);
TO: 2;

FROM: 2;
  assume(x>y);
TO: 3;
*)

let rec state1 x y =
  let rho = read_int () in
  if rho > 0 then
    state1 (x+1) (y+1)
  else
    ()
let main () =
  state1 0 0; assert false