let rec append xs ys =
  match xs with
  | [] -> ys
  | x::xs' -> x :: append xs' ys

let rec make_list n =
  if n < 0
  then []
  else Random.int 0 :: make_list (n-1)

let main n i =
  let xs = make_list n in
  let ys = make_list n in
  let zs = make_list n in
  let xsys = append xs ys in
  let xsyszs1 = append xsys zs in
  let yszs = append ys zs in
  let xsyszs2 = append xs yszs in
  assert (List.nth xsyszs1 i = List.nth xsyszs2 i)
