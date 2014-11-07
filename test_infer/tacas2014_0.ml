(* Chen et al, TACAS 2014 *)
let rec f i =
  if i>=0 then
    let next = read_int () in f next
  else 2
let main () =
  let k = read_int () in
  let init = if k >=0 then read_int () else -1 in
  f init; assert false
