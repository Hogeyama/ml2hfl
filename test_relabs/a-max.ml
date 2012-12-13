(*
let rec loop x = loop x

let make_array n =
  let rec aux i array =
    if i >= n
    then array
    else
      let x = Random.int 0 in
        aux (i+1) (fun j -> if j = i then x else array j)
  in
    aux 0 loop

let rec array_max i n array =
  if i >= n
  then 0
  else
    let x = array i in
    let m' = array_max (i+1) n array in
      if x > m' then x else m'

let main i n =
  if 0 <= i && i < n
  then
    let array = make_array n in
    let m = array_max 0 n array in
      assert (array i <= m)
*)
let rec loop x = loop x

let make_array n =
  let rec aux i array =
    if i >= n
    then array
    else
      let x = Random.int 0 in
        aux (i+1) (fun j -> if j = i then x else array j)
  in
    aux 0 loop

let rec array_max i n array =
  if i >= n
  then 0, loop
  else
    let x = array i in
    let m',array' = array_max (i+1) n array in
      (if x > m' then x else m'), fun j -> if j = i then x else array' j

let main i n =
  if 0 <= i && i < n
  then
    let array = make_array n in
    let m,array' = array_max 0 n array in
      assert (array' i <= m)
