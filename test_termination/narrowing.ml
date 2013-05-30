let rec f b n m =
  if b then
    f true (n-1) m
  else
    f false n (m-1)
in
f true (Random.int 0) (Random.int 0)
