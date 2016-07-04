let make_vect n s = n, fun i -> assert (0 <= i && i < n); s
let get (n,map) i = map i
let update (n,map) i x = n, (map i; fun j -> if i = j then x else map j)

let assign queenArray i j = update queenArray i j

let rec dotsPrint n = if n = 0 then () else begin print_string "."; dotsPrint (n-1) end
let queenPrint size queenArray =
  let rec aux row = begin
      if row = size then () else
        let n = get queenArray row in
        dotsPrint(n-1); print_string "Q"; dotsPrint(size - n); print_string "\n"; aux (row + 1)
    end
  in
  aux(0); print_string "\n"

let test j queenArray =
  let qj = get queenArray j in
  let rec aux i =
    if i < j then
      let qi = get queenArray i in
      if qi = qj then false else if abs(qj - qi) = j - i then false else aux (i+1)
    else true
  in aux 0

let rec loop row size queenArray =
  let next = get queenArray row + 1 in
  if next > size then
    let queenArray = assign queenArray row 0 in
    if row = 0 then () else loop (row-1) size queenArray
  else
    let queenArray = assign queenArray row next in
    if test row queenArray then
      if (row+1) = size then begin queenPrint size queenArray; loop(row) size queenArray end else loop(row+1) size queenArray
    else loop row size queenArray

let main size =
  let queenArray = make_vect size 0 in
  if size > 0 then
    loop(0) size queenArray
