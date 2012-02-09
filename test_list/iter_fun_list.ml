let rec iter (f:(int->unit)->unit) xs =
  match xs with
      [] -> ()
    | x::xs' -> f x; iter f xs'

let rec make_list n =
  if n < 0
  then []
  else (fun x -> assert (n >= x)) :: make_list (n-1)

let main n =
  let xs = make_list n in
    iter (fun f -> f 0) xs



