(*
let rec map f xs =
  match xs with
    [] -> []
  | x::xs' -> f x :: map f xs'

let rec loop () = loop ()

let rec nth i xs =
  match xs with
    [] -> loop ()
  | x::xs' ->
    if i = 0
    then x
    else nth (i-1) xs'

let succ x = x + 1

let main i xs =
  let xs' = map succ xs in
  assert (succ (nth i xs) = nth i xs')
*)

(*
let rec map f xs =
  match xs with
    [] -> []
  | x::xs' -> f x :: map f xs'

let rec loop () = loop ()

let rec nth i xs =
  match xs with
    [] -> loop ()
  | x::xs' ->
    if i = 0
    then x
    else nth (i-1) xs'

let succ x = x + 1

let main i xs =
  let xs' = map succ xs in
  assert (succ (nth i xs) = nth i xs')
*)

let rec loop () = loop ()

let rec map f (l,xs) =
  if l = 0
  then 0, loop ()
  else
    let x = xs 0 in
    let xs' i = xs (i+1) in
    let x' = f x in
    let l',xs'' = map f (l-1,xs') in
    let ys i =
      if x = 0
      then x'
      else xs'' (i - 1)
    in
    l'+1,ys

let rec nth i (l,xs) =
  if l = 0
  then loop ()
  else
    let x = xs 0 in
    let xs' i = xs (i+1) in
    if i = 0
    then x
    else nth (i-1) (l-1,xs')

let succ x = x + 1

let main i (l,xs) =
  let l',xs' = map succ (l,xs) in
  assert (succ (nth i (l,xs)) = nth i (l',xs'))
