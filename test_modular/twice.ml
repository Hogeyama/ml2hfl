
let rec twice f x k =
  let k' r = f r k in
  f x k'

let succ x k = k (x + 1)

let main n =
  let k0 r = assert (n <= r) in
  twice succ n k0
