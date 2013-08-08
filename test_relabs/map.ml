(*
let rec map f xs =
  match xs with
    [] -> []
  | x::xs' -> f x :: map f xs'

let succ x = x + 1
let double x = 2 * x

let le_list xs ys =
  match xs with
    [] -> true
  | x::xs' ->
    match ys with
      [] -> true
    | y::ys' -> x <= y && le_list xs' ys'

let main xs =
  let xs' = map succ xs in
  let xs'' = map double xs in
  le_list xs' xs''
*)


let rec map f xs =
  match xs with
    [] -> []
  | x::xs' -> f x :: map f xs'

let succ x = x + 1
let double x = 2 * x

let le_list xs ys =
  match xs with
    [] -> true
  | x::xs' ->
    match ys with
      [] -> true
    | y::ys' -> x <= y && le_list xs' ys'

let main xs =
  let xs' = map succ xs in
  let xs'' = map double xs in
  le_list xs' xs''
