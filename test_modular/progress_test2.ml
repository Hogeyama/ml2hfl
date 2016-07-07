
let rec loop queenArray =
  if Random.bool() then
    loop queenArray

let main size =
  let queenArray i = assert false in
  loop queenArray
