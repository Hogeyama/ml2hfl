let twice f x = f (f x) in
let neg x = -x in
let neg2 x = -2*x in
  if n>=0
  then
     let x = twice neg n in
     let y = twice neg2 n in
       assert (x>=0 && y>=0)
  else ()

