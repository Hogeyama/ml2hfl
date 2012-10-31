
(*
let rec filter f xs =
  match xs with
      [] -> []
    | x::xs' ->
        if f x
        then x :: filter f xs'
        else filter f xs'
*)

let f n = n < 0 in
let rec filter f n =
  if n = 0
  then 0
  else
    if ***
    then 1 + filter f (n-1)
    else filter f (n-1)
in
let n = ?n? in
  if filter f n <= n
  then ()
  else fail ()

