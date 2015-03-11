let rec f m n =
  if n < 0 then
    if m < 0 then ()
    else f (m+1) m
  else f m (n-1)
let main () =
  let m = read_int () in
  let n = read_int () in
  f m n; assert false
