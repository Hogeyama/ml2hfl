let is_mono f =
  let n = Random.int 0 in
  let m = Random.int 0 in
  if n < m then f n <= f m else true

let rec sum n =
  if n <= 0
  then 0
  else
    let s = sum (n - 1) in
    n + s

let main n =
  assert (is_mono sum)
