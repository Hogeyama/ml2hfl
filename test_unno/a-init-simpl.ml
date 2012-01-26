let make_array n i = 0
let rec init i n a =
  if i >= n then a else init (i + 1) n a
let main n i =
  let x = init 0 n (make_array n) in
    if 0 <= i && i < n then
      assert (x i >= 1)
