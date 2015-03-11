let rec f m n =
  if m <> n
    then f m (n-2)
    else ()
let main () =
  let m = read_int () in
  let n = read_int () in
  (if (m < n)
    then f m n
    else ()); assert false
