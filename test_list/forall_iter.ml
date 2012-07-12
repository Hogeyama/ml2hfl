
let rec make_list n =
  if n < 0
  then []
  else Random.int 100 :: make_list (n-1)

let rec for_all f xs =
  match xs with
      [] -> true, []
    | x::xs' ->
        let b,xs' = for_all f xs' in
          f x && b, x::xs'


let rec iter f xs =
  match xs with
      [] -> ()
    | x::xs' -> f x; iter f xs'

let main n =
  let xs = make_list n in
  let b,xs' = for_all (fun n -> n = 0) xs in
    if b then iter (fun n -> assert (n = 0)) xs'
