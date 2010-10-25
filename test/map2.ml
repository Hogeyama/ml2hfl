let rec map x =
  if x = 0
  then 0
  else 1 + map (x-1)
in
  assert (map n >= n)
