let main n m =
  let xs = make_list n in
  let ys = make_list m in
  assert (list_eq (append (rev xs) (rev ys)) (rev (append xs ys)))
