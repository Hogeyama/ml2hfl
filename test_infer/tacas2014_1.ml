(* Chen et al, TACAS 2014 *)
(* "Tricky example" in Section 4 (whose termination cannot be proved by TNT [Gupta et al. POPL 2008]) *)
let rec g j =
  if j >= 1 then
    g (j-1)
  else j
let rec f k j =
  if k >= 0 then
    let k2 = k + 1 in
    f k2 (g k2)
  else ()
let main () =
  let k = read_int () in
  let j = read_int () in
  f k j
