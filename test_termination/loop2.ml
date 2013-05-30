let rec f n m =
  if n > 0 then
    f (n-1) m
  else if m > 0 then
    f n (m-1)
  else
    ()
in
f (Random.int 0) (Random.int 0)
