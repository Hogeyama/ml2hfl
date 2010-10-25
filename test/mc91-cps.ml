let rec m x k =
  if x > 100
  then k (x-10)
  else m (x+11) (let f r = m r k in f)
in
let c y r = if y<=101 then if r=91 then () else fail () else () in
let id x = x in
let n = id ?n? in
  m n (c n)
