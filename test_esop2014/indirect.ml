let id (x:unit) = x
let app h (v : int) (u : unit) = (h v u : unit)
let rec g (x : int) =
  if x > 0 then
    app g (x-1)
  else
    id
let main (u:unit) = g (Random.int 0) ()
