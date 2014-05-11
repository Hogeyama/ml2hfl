
(*{SPEC}

(* "i <= j && j < n" should not be splitted because of cartesian abstraction *)
valcps array_max :
  i:int ->
  n:int ->
  (int->(int->X)->X) ->
  (m:int -> (j:int-> (r:int[not i <= j; not j < n] -> X) -> X) -> X) -> X

{SPEC}*)

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
  if i < 0 then loop () else
  if i >= n
  then 0
  else
    let m = array_max (i+1) n array in
    let x = array i in
    if x > m then x else m

let main i n =
  if 0 <= i && i < n
  then
    let array = make_array n in
    let m = array_max 0 n array in
    assert (array i <= m)
