let rec append (len1, l1) (len2, l2) =
  if len1 = 0 then
    (len2, l2)
  else
    let (len, l) = append (len1 - 1, fun i -> l1 (i + 1)) (len2, l2) in
    (len + 1, fun i -> if i = 0 then l1 0 else l (i - 1))
let apply f x = f x
let main len1 len2 =
  apply (fun (len, _) -> assert (len <= len1 + len2)) (append (len1, fun i -> true) (len2, fun i -> false))
