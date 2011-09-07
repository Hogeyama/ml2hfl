let rec length s (xs:int list) =
  match xs with
      [] -> s
    | _::xs' -> 1 + length s xs'

let rec make_list n =
  if n = 0
  then []
  else n :: make_list (n-1)

let main n m =
  let xs = make_list n in
  let x = length m xs in
  let y = n + m in
    assert (x<=y && x>=y)
