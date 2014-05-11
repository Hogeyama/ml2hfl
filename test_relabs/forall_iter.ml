
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
