let rec gib a b n =
  if n=0 then a
  else if n=1 then b
  else gib a b (n-1) + gib a b (n-2)
in
  assert (gib 0 1 n >= 0)

