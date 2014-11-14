let rec f n =
  if n<0 then ()
  else
    let r = read_int () in
    if r>0 then f (n-1) else f (n+1)
let main () =
  let r = read_int () in
  f r; assert false
