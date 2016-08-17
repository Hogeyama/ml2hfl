
let rec dotsPrint n = if n = 0 then () else begin print_string "."; dotsPrint (n-1) end
let queenPrint size queenArray =
  let get i n map = map i in

  let rec aux row = begin
      if row = size then () else
        begin
          assert (0 <= row && row < size);
          let m = get row size queenArray in
          dotsPrint(m-1); print_string "Q"; dotsPrint(size - m); print_string "\n"; aux (row + 1)
        end
    end
  in
  aux(0); print_string "\n"

let test j n queenArray =
  let get i n map = map i in
  assert (0 <= j && j < n);
  let qj = get j n queenArray in
  let rec aux i =
    if i < j then
      let () = assert (0 <= i && i < n) in
      let qi = get i n queenArray in
      if qi = qj then false else if abs(qj - qi) = j - i then false else aux (i+1)
    else true
  in aux 0

let rec loop row size queenArray =
  let get i n map = map i in
  let update i x n map = fun j -> if i = j then x else map j in

  let assign n queenArray i j =
    update i j n queenArray
  in
  assert (0 <= row && row < size);
  let next = get row size queenArray + 1 in
  if next > size then
    let queenArray = assign size queenArray row 0 in
    if row = 0 then () else loop (row-1) size queenArray
  else
    let queenArray = assign size queenArray row next in
    if test row size queenArray then
      if (row+1) = size then begin queenPrint size queenArray; loop(row) size queenArray end else loop(row+1) size queenArray
    else loop row size queenArray

let main size =
  let make_vect n s = fun i -> s in

  let queenArray = make_vect size 0 in
  if size > 0 then
    loop(0) size queenArray
