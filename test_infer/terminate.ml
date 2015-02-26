let rec f n = if n > 0 then f 0 else ()
let main () = 
  let r = read_int () in
  f r
