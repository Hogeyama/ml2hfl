(* forall_leq_n.ml: verification failed *)
let rec make_list n =
  if n < 0 then
    []
  else
    n :: make_list (n-1)

let check n x = n >= x

let main n =
		let rec for_all f (xs:int list) =
		  match xs with
		    [] ->
		      true
		  | x::xs' ->
		      f x && for_all f xs'
  in
  assert (for_all (check n) (make_list n))
