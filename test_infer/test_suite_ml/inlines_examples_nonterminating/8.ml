(*
START: 0;

FROM: 0;
TO: 10;

FROM: 10;
   rho := nondet();
TO: 11;
         
FROM: 11;
  assume(rho>0);
TO: 12;

FROM: 11;
  assume(rho<=0);
TO: 13;
                    
FROM: 12;
  assume(x>0);
  x := x-1;
TO: 10;

FROM: 13;
  assume(x>0);
  x := nondet();
TO: 10;
*)
let rec state10 x =
  let rho = read_int () in
  if rho > 0 then
    if x>0 then state10 (x-1) else ()
  else
    if x>0 then
      let x_next = read_int () in
      state10 x_next
    else ()
let main () =
  let x = read_int () in
  state10 x; assert false
