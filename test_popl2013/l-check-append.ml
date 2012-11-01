(* untypable‚Á‚Û‚¢‚ªCPS•ÏŠ·‚É‚æ‚Á‚ÄŒŸØ‚Å‚«‚Ä‚¢‚é *)
let rec append (len1, l1) (len2, l2) =
  if len1 = 0 then
    (len2, l2)
  else
		  let (len, l) = append (len1 - 1, fun i -> l1 (i + 1)) (len2, l2) in
    (len + 1, fun i -> if i = 0 then l1 0 else l (i - 1))
let check (len, l) x = assert (len <= x)
let main len1 len2 =
  check (append (len1, fun i -> true) (len2, fun i -> false)) (len1 + len2)
