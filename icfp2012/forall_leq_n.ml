let rec for_all f (xs:int list) =
  match xs with
      [] -> true
    | x::xs' -> f x && for_all f xs'

let rec make_list n =
  if n < 0
  then []
  else n :: make_list (n-1)

let main n =
  let check x = n >= x in
    assert (for_all check (make_list n))
