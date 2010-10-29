let c q = () in      (* c q -> .  for any q *)
let b x q = x 1 in  (* b q -> q1 for any q *)
let a x y q = if q=0 then (x 0; y 0) else fail() in
    (* a q0 -> q0 q0 *)
let rec f n x q = if n<=0 then x q else a x (f (n-1) (b x)) q
    (* F n x = if n<=0 then x else a x (f (n-1) (b x)) *)
in
let s q = f n c q in
    (* S -> F n c *)
  s 0
     (* check whether S: q0 *)
