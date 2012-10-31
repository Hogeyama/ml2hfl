let twice f x = f (f x) in
let f x = 2 * x in
let id x = x in
let n = id ?n? in
  if n >= 0
  then
    if twice f n > n
    then ()
    else fail ()
  else ()
