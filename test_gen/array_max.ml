(* non-linear expression not supported *)
let make_array n i = assert (0 <= i && i < n); i*i
let rec array_max n i a =
  if i >= n then 0
  else
    let x = a i in
      let y = array_max n (i+1) a in
        if x > y then x else y in
let rec check n i max a =
  if i >= n then ()
  else (assert (a i <= max); check n (i+1) max a)
let main n =
  let array = make_array n
  let max = array_max n 0 array
  check n 0 max array
