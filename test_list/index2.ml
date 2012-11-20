let rec make_list i n =
  if i > n
  then []
  else i :: make_list (i+1) n

let rec check i xs =
  match xs with
      [] -> ()
    | x::xs' -> assert (i=x); check (i+1) xs'

let main min max =
 check min (make_list min max)
