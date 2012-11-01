let rec reverse (len1, l1) (len2, l2) =
  if len2 = 0 then
    l1
  else
    reverse (len1 + 1, fun i -> if i = 0 then l2 0 else l1 (i - 1)) (len2 - 1, fun i -> l2 (i + 1))

let main len =
  if len > 0 then
    let _ = reverse (0, fun i -> assert false) (len, fun x -> 0) 0 in ()
