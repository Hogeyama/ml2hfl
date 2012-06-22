let rec append_cps (*ex*) k (len1, l1) (len2, l2) =
  if len1 = 0 then
    k (len2, l2)
  else
    append_cps
				  (*(len1 + len2)*)
				  (fun (len, l) -> k (len + 1, fun i -> if i = 0 then l1 0 else l (i - 1)))
				  (len1 - 1, fun i -> l1 (i + 1))
						(len2, l2)
let rec length (len, l) =
  if len = 0 then
		  0
		else
		  1 + length (len - 1, fun i -> l (i + 1))
let main len1 len2 =
		append_cps
		  (*(len1 + len2)*)
				(fun (len, l) -> assert (length (len, l) = len1 + len2))
		  (len1, fun i -> true)
				(len2, fun i -> false)
