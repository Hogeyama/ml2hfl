let make_array n i = assert (0 <= i && i < n); 0 in
let update i a x j = if j>i-1 && j<=i then x else a(j) in
let rec init i n a =
  if i>=n then a
  else init (i+1) n (update i a 1)
in
  if k>=0 && k<=0 then 
    let x = init k n (make_array n) in
      if 0<=i && i<n then
        assert (x i >= 1)
      else ()
  else ()

