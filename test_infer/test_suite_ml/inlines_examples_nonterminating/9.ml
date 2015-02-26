(*
START: 0;

FROM: 0;
TO: 5;

FROM: 5;
  assume(x>0);
  y := x;
TO: 3;
         
FROM: 3;
  assume(y>0);
TO: 4;

FROM: 4;
  y := y-1;
TO: 3;
                    
FROM: 3;
TO: 2;

FROM: 2;
  x := x-0;
  y := y+1;
TO: 5;

FROM: 2;
  y := y+1;
TO: 5;
*)
let rec state5 x y =
  if x>0 then state3 x x
  else ()
and state3 x y =
  let br = read_int () in
  if br > 0 && y > 0 then state3 x (y-1)
  else state2 x y
and state2 x y =
  let br = read_int () in
  if br > 0 then state5 (x-0) (y+1)
  else state5 x (y+1)
let main () =
  let x = read_int () in
  let y = read_int () in
  state5 x y; assert false
