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

let rec loop x = loop ()
let assume b = if b then () else loop ()

let make_array n =
  let init x = 0 in
  let rec aux i array =
    if i >= n
    then array
    else
      let x = Random.int 0 in
      aux (i+1) (fun j -> if j = i then x else array j)
  in
    aux 0 init

let rec array_max i n arrayk =
  if i >= n
  then
    let array i = fst (arrayk i 0 (fun x -> x)) in
    let k m a = snd (arrayk 0 m a) in
    k 0 array
  else
    let x = fst (arrayk i 0 (fun x -> x)) in
    array_max (i+1) n (fun j m' array'' ->
    (if j = i then x else fst (arrayk j 0 (fun x -> x))),
    snd (arrayk 0 (if x > m' then x else m') array''))

let main i n =
  if 0 <= i && i < n
  then
    let array = make_array n in
    array_max 0 n (fun i m array' -> array' i,
      assert (array' i <= m))
