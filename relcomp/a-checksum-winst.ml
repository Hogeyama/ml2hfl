(* safe but untypable *)
let make n i = assert (0 <= i && i < n); 0
let update n ex ar i x =
  assert (0 <= i && i < n);
  fun j -> if j = i then x else ar j
let checksum ex ar x = assert ((ar 0) + (ar 1) = x)
let main a b = 
  let ar = update 2 (a + b) (update 2 a (make 2) 0 a) 1 b in
  checksum (a + b) ar (a + b)
