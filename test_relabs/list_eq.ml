(*
let rec append xs ys =
  match xs with
    [] -> ys
  | x::xs' -> x :: append xs' ys

let rec rev xs =
  match xs with
    [] -> []
  | x::xs' -> append (rev xs') [x]

let rec make_list n =
  if n < 0
  then []
  else Random.int 0 :: make_list n

let rec list_eq xs ys =
  match xs, ys with
    0, 0 -> true
  | x::xs', y::ys' -> x = y && list_eq xs' ys'
  | _ -> false

let main n =
  let xs = make_list n in
  assert (list_eq xs (rev (rev xs)))
*)

let rec loop x = loop x

let rec append (l1,xs) (l2,ys) =
  if l1 = 0 then (l2,ys)
  else if l1 > 0 then
    let x = xs 0 in
    let xs' i = xs (i+1) in
    let l3,zs = append (l1-1,xs') (l2,ys) in
    let zs' i = if i = 0 then x else zs (i-1) in
    l3+1, zs'
  else loop ()

let rec rev (l,xs) =
  if l = 0
  then (0, fun i -> loop ())
  else
    let x = xs 0 in
    let xs' i = xs (i+1) in
    append (rev (l-1,xs')) (1, fun i -> if i = 0 then x else loop ())

let rec make_list n =
  if n <= 0
  then (0, fun i -> loop ())
  else
    let x = Random.int 10 in
    let l,xs = make_list (n-1) in
    l+1, fun i -> if i=0 then x else xs (i-1)

let rec list_eq (l1,xs) (l2,ys) =
  if l1 = 0 && l2 = 0
  then true
  else
    if l1 > 0 && l2 > 0
    then
      let x = xs 0 in
      let y = ys 0 in
      let l1',xs' = l1-1, fun i -> xs (x+1) in
      let l2',ys' = l2-1, fun i -> ys (x+1) in
      x = y && list_eq (l1',xs') (l2',ys')
    else l1 < 0 || l2 < 0

let main n =
  let l,xs = make_list n in
  assert (list_eq (l,xs) (rev (rev (l,xs))))
