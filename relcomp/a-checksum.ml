let make_array n = (n, fun i -> assert (0 <= i && i < n); 0)
let update (*ex1*) (*ex2*) (n, ar) i x =
  assert (0 <= i && i < n);
  (n, fun j -> if j = i then x else ar j)

let checksum (*ex1*) (*ex2*) (n, ar) x = assert ((ar 0) + (ar 1) = x)
let main a b = 
  checksum (*a*) (*b*) (update (*a*) (*b*) (update (*a*) (*0*) (make_array 2) 0 a) 1 b) (a + b)
