let rec to_even x =
  if x = 0 || x = 1
  then 0
  else 2 + to_even (x-2)

let main n =
  let e = to_even n in
    assert (to_even e = e)
