(*
let rec forall f xs =
  match xs with
      [] -> true
    | x::xs' ->
        if f x
        then forall f xs'
        else false

let rec iter f xs =
  match xs with
      [] -> ()
    | x::xs' -> f x; iter f xs'

let geq0 x = x >= 0

let main xs =
  if forall geq0 xs
  then iter (fun x -> assert (geq0 x)) xs

*)
let rec forall f xs =
  match xs with
      [] -> true, []
    | x::xs' ->
        if f x
        then
          let r,xs'' = forall f xs' in
            r, x::xs''
        else false, x::xs'

let rec iter f xs =
  match xs with
      [] -> ()
    | x::xs' -> f x; iter f xs'

let geq0 x = x >= 0

let main xs =
  let r, xs' = forall geq0 xs in
    if r
    then iter (fun x -> assert (geq0 x)) xs'

let rec make_list () =
  if Random.bool ()
  then []
  else (Random.int 0)::make_list ()

let main () = main (make_list ())
