let rec length_aux acc (xs:int list) =
  match xs with
      [] -> acc
    | _::xs' -> length_aux (acc+1) xs'
let length xs = length_aux 0 xs

let rec make_list n =
  if n = 0
  then []
  else n :: make_list (n-1)

let main n =
  let xs = make_list n in
    assert (length xs = n)




