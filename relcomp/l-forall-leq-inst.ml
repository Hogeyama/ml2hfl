(* verification succeeded *)
let rec loop (u:unit) = loop u
(*let hd (len, l) = l 0*)
(*let tl (len, l) = (len - 1, fun i -> l (i + 1))*)
(*let nil = (0, fun i -> loop ())*)
(*let cons a (len, l) = (len + 1, fun i -> if i = 0 then a else l (i - 1))*)

let rec make_list n =
  if n < 0 then
    (0, fun i -> loop ())(*nil*)
  else
		  let (len, l) = make_list (n - 1) in
    (len + 1, fun i -> if i = 0 then n else l (i - 1))(*cons n (len, l)*)

let leq n x = x <= n

let rec for_all ex f (len, l) =
		if len = 0 then
	   true
  else if len >= 1 then
	   f (l 0(*hd (len, l) fails*)) && for_all ex f (len - 1, fun i -> l (i + 1))(*(tl (len, l))*)
  else
    loop ()

let main len =
  assert (for_all len (leq len) (make_list len))
