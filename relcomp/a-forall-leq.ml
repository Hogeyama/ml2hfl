let rec for_all f (sz, ar) =
		if sz = 0 then
	   true
  else
	   f (ar 0) && for_all f (sz - 1, fun i -> ar (i + 1))

let main sz =
  assert (for_all (fun x -> x <= sz) (sz, fun i -> sz - i))
