let rec length_aux acc (xs:int list) =
  match xs with
      [] -> acc
    | _::xs' -> length_aux (acc+1) xs'

let rec make_list n =
  if n = 0
  then []
  else n :: make_list (n-1)

let main n m =
  let xs = make_list n in
  let x = length_aux m xs in
  let y = n + m in
    assert (x<=y && x>=y)




