let rec append (len1, l1) (len2, l2) =
  if len1 = 0 then
    (len2, l2)
  else
    let (len, l) = append (len1 - 1, fun i -> l1 (i + 1)) (len2, l2) in
    (len + 1, fun i -> if i = 0 then l1 0 else l (i - 1))
let rec length_cps (*ex*) k (len, l) =
  if len = 0 then
		  k 0
		else
		  length_cps
				  (*len‚©ex-1‚ª•K—v‚È‚Ì‚Éex‚ª‹‚Ü‚Á‚Ä¸”s app-succ‚Æ“¯‚¶Œ»Û*)
				  (fun len -> k (len + 1))
						(len - 1, fun i -> l (i + 1))
let main len1 len2 =
  length_cps
		  (*(len1 + len2)*)
		  (fun len -> assert (len <= len1 + len2))
		  (append (len1, fun i -> true) (len2, fun i -> false))
