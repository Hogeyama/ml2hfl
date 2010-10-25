let open x = 1 in
let close fp = 0 in
let read fp = if fp = 0 then fail () else () in
let rec read_n fp n =
  if n <= 0
  then close fp
  else
    (read fp;
     read_n fp (n-1))
in
let n = ?n? in
let fp = open () in
let fp = read_n fp n in
  if fp = 0 then () else fail ()

