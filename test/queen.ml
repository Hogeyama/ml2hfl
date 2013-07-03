let make_array n s i = assert (0 <= i && i < n); s
let update i n a x = a i; let a j = if i=j then x else a j in a

let print_string s = ()
let abs x = if x < 0 then -x else x

let queen size =
  let queenArray = make_array size 0 in
  let assign i j a = update i size a j in
  let rec dotsPrint n = if n = 0 then () else begin print_string (); dotsPrint (n-1) end in
  let queenPrint a =
    let rec aux row a =
      if row >= size then () else
        let n = a row in
          dotsPrint (n-1); print_string (); dotsPrint(size - n); print_string (); aux (row + 1) a
    in
      aux 0 a; print_string ()
  in
  let test j a =
    let qj = a j in
    let rec aux i =
      if i < j then
        let qi = a i in
          if qi = qj then false else if abs(qj - qi) = j - i then false else aux (i+1)
      else true
    in
      aux 0
  in
  let rec loop row a =
    let next = a row + 1 in
      if next > size then
        begin
          let a = assign row 0 a in
            if row = 0 then () else loop (row-1) a end
      else
      begin let a = assign row next a in
        if test row a then
          if row > size then begin queenPrint a; loop(row) a end else loop(row+1) a
          else loop row a
      end
  in
    loop 0 queenArray

let main n =
  if n>0 then (queen n; ()) else ()
