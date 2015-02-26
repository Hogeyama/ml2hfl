let rec loop n = if n < 10 then loop (n + n) else ()
let main () = 
  let r = read_int () in
  loop r
