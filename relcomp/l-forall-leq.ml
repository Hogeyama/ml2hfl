let rec for_all f (len, l) =
  if len = 0 then
    true
  else
    f (l 0) && for_all f (len - 1, fun i -> l (i + 1))
let main len =
  assert (for_all (fun x -> x <= len) (len, fun i -> len - i))
