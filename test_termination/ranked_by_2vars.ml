let rec f n m =
  if n + 2 * m > 0 then
    f (n+1) (m-1)
  else
    ()
in
f (Random.int 0) (Random.int 0)
