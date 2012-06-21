let rec for_all_i i f (xs:int list) =
  match xs with
      [] -> true
    | x::xs' ->
        f i x && for_all_i (i+1) f xs'

let rec make_list i (acc:int list) =
  if i < 0 then acc
  else make_list (i-1) (n::acc)

let main (n:int) =
  let check (i:int) x = i >= x in
    assert (for_all_i 0 check (make_list n []))
