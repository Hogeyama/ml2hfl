let rec add x y =
  if x = 0 then
    y
  else
    1 + add (x-1) y
in
assert (add n n >= 2 * n)
