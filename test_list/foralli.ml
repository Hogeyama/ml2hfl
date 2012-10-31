
let rec for_all_i i f xs =
  match xs with
      [] -> true
    | x::xs' ->
        f i x && for_all_i (i+1) f xs'

let rec make_list acc n =
  if n < 0
  then acc
  else make_list (n::acc) (n-1)

let main n =
  let check i x = i >= x in
    assert (for_all_i 0 check (make_list [] n))



