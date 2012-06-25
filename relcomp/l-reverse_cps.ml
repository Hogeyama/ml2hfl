let rec reverse_cps k (len1, l1) (len2, l2) =
  if len2 = 0 then
    k (len1, l1)
  else
    reverse_cps
      k
      (len1 + 1, fun i -> if i = 0 then l2 0 else l1 (i - 1))
      (len2 - 1, fun i -> l2 (i + 1))

let main len =
  if len > 0 then
    reverse_cps
      (fun (_, l) -> let _ = l (len - 1) in ())
      (0, fun i -> assert false)
      (len, fun x -> 0)
