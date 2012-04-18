(* verification failed *)
let make n i = 0
let update i x ar j = if j = i then x else ar j
let rec init i x n ar =
  if i >= n then ar else init (i + 1) x n (update i x ar)
let main x n i =
  if 0 <= i && i < n then
    let ar = init 0 x n (make n) in
    assert (ar i = x)
