
let test j n queenArray =
  Random.bool()

let rec loop row size n queenArray =
  assert (row < n);
  let queenArray j = if row = j then 0 else queenArray j in
  if test row n queenArray then
    if (row+1) = size then
      loop (row) size n queenArray
    else
      loop (row+1) size n queenArray
  else
    ()

let main size =
  let queenArray i = 0 in
  if size > 0 then
    loop (0) size size queenArray
