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

let rec array_max i n array k =
  if i >= n
  then k 0 array
  else
    array_max (i+1) n array (fun m array' ->
    let x = array' i in
    let array'' j = if j = i then x else array' j in
    k (if x > m then x else m) array'')

let main i n =
  if 0 <= i && i < n
  then
    let array = make_array n in
    array_max 0 n array (fun m array' ->
    assert (array' i <= m))
