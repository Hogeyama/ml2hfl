
let max_int = 1000

let rec max_list xs =
  match xs with
      [] -> max_int
    | x::xs' ->
        let m = max_list xs' in
          if m > x
          then m
          else x

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
  let m = max_list xs in
    if 0 <= i && i <= n-1
    then assert (nth i xs >= m || m = max_int)
    else ()


