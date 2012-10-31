let open x = true in
let close fp = false in
let read fp = if fp then () else fail () in
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
  if fp then fail () else ()

