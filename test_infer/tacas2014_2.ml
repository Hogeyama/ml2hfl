(* Chen et al, TACAS 2014 *)
(* Generalization of predicates may be necessary in [TACAS 2014]'s method *)
let rec f i =
  if i >= 0 then
    let k = read_int () in
    if k >= 0 then
      f (i-1)
    else
      f i
  else
    ()
let main () =
  let i = read_int () in
  f i
