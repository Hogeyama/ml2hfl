let rec append (len1, l1) (len2, l2) =
  if len1 = 0 then
    (len2, l2)
  else
    let (len, l) = append (len1 - 1, fun i -> l1 (i + 1)) (len2, l2) in
    (len + 1, fun i -> if i = 0 then l1 0 else l (i - 1))
let rec length (len, l) =
  if len = 0 then
		  0
		else
		  1 + length (len - 1, fun i -> l (i + 1))
let apply_cps (*ex*) k f x = k (f x)
let main len1 len2 =
  apply_cps (*(len1 + len2)*)(*len2‚±‚ê‚ÅSHP‚ÉŒ^•t‚¢‚Ä‚µ‚Ü‚¤*) (fun len -> assert (len >= len1 + len2)) length (append (len1, fun i -> true) (len2, fun i -> false))
