let rec insert (x:int) ys =
  match ys with
  | [] -> [x]
  | y::ys' ->
      if x < y then x::ys
      else y::(insert x ys')

let rec insertsort xs =
  match xs with
  | [] -> []
  | x::xs' ->
      insert x (insertsort xs')

let rec make_list n =
  if n = 0
  then []
  else n :: make_list (n-1)

let main n i j =
  let xs = make_list n in
  let ys = insertsort xs in
  if i < j
  then assert (List.nth ys i <= List.nth ys j)
