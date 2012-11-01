let make_array n = (n, fun i -> assert (0 <= i && i < n); 0)
let update (*ex1*) (*ex2*) (n, ar) i x =
  assert (0 <= i && i < n);
  (n, fun j -> if j = i then x else ar j)

let check (*ex1*) (*ex2*) (n, ar) i x = assert (ar i = x)
let main n i x =
  if 0 <= i && i < n then
    check (*i*) (*x*) (update (*0*) (*0*) (make_array n) i x) i x
