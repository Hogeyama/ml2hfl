let twice f x = f (f x) in
let neg x = -x in
  if n>=0
  then
    let x = twice neg n in
      assert (x>=0)
  else ()

