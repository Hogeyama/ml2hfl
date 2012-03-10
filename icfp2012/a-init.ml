let make n i = 0
let update i x ar j = if j = i then x else ar j
let rec init i n ar =
  if i >= n then ar else init (i + 1) n (update i 1 ar)
let main n i =
  if 0 <= i && i < n then
    let ar = init 0 n (make n) in
    assert (ar i = 1)
