let open x = true in
let close fp = false in
let read fp = if fp then () else fail () in
let rec read_n fp n m =
  if n <= 0
  then
    if m > 0
    then close fp
    else fp
  else
    ((if m > 0 then read fp else ());
     read_n fp (n-1) m)
in
let n = ?n? in
let m = ?m? in
let fp = if m > 0 then open () else false in
let fp = read_n fp n m in
  if fp then fail () else ()

