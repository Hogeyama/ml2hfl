let make n = (n, fun i -> assert (0 <= i && i < n); 0)
let update (n, ar) i x =
  assert (0 <= i && i < n);
  (n, fun j -> if j = i then x else ar j)
let checksum (n, ar) x = assert ((ar 0) + (ar 1) = x)
let main a b =
  checksum (update (update (make 2) 0 a) 1 b) (a + b)
