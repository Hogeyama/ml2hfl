let rec m x =
  if x <= 100
  then m (m (x + 11))
  else x - 10
in
let id x = x in
let n = id ?n? in
  if n <= 102
  then
    if m n = 91
    then ()
    else fail ()
  else ()

