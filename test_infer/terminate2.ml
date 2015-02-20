let rec f n = if n >= 0 then () else f (-n)
let main () = 
  let r = read_int () in
  f r
