
let rec max_list (acc:int) xs =
  match xs with
      [] -> acc
    | x::xs' ->
        if x > acc
        then max_list x xs'
        else max_list acc xs'

let rec make_list n =
  if n < 0
  then []
  else n :: make_list (n-1)

let rec nth n (xs:int list) =
  match xs with
    | [] -> let rec loop (u:unit) : int = loop u in loop ()
    | x::xs' -> if n = 0 then x else nth (n-1) xs'

let main n i =
  let xs = make_list n in
  let m = max_list 0 xs in
    if 0 <= i && i <= n-1
    then assert (nth i xs >= m)
    else ()


