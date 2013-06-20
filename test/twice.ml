let twice f x = f (f x) in
let f x = 2 * x in

let main n =
  if n > 0
  then assert (twice f n > n)
