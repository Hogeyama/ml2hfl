let make n = (n, fun i -> assert (0 <= i && i < n); 0)
let update (n, ar) i x =
  assert (0 <= i && i < n);
  (n, fun j -> if j = i then x else ar j)
let check (n, ar) i x = assert (ar i = x)
let main n i x =
  if 0 <= i && i < n then
    check (update (make n) i x) i x
