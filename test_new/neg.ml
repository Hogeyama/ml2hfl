let g x y = x
let twice f x y = f (f x) y
let neg x y = - x ()
let main n =
  if n>=0 then
    let z = twice neg (g n) () in
    assert (z>=0)
