let rec array_max n i =
  if i >= n
  then n
  else
    let x = i in
    let y = array_max n (i+1) in
      if x > y then x else y
in
let rec check n i max =
  if i >= n
  then ()
  else
    begin
      assert (i <= max);
      check n (i+1) max
    end
in
let max = array_max n 0 in
  check n 0 max
