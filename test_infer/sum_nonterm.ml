let rec sum n =
  if n = 0 then 0
  else n + sum (n - 1)
let main () =
  let r = read_int () in
  sum r;
  assert false
