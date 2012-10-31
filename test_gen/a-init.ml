let make_array n i = assert (0 <= i && i < n); 0
let update i a x j :int= if j > i-1 && j <= i then x else a (j)
(*j=1‚É‚·‚é‚Æ-enr -rsn 0‚Åcsisat error*)
let rec init i n a =
  if i >= n then a else init (i + 1) n (update i a 1)
let main n i =
  let x = init 0 n (make_array n) in
  if 0 <= i && i < n then
    assert (x i >= 1)
