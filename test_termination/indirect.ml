let app h (v : int) (u : unit) = (h v u : unit) in
let g (u : unit) = () in
let rec f (x : int) =
  if x > 0 then
    app f (x-1)
  else
    g
in f (Random.int 0) ()
