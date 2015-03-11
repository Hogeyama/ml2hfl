let rec app f (x:int) (u : unit) = if x>0 then app f (x-1) u else (f x () : unit)
let id (u : unit) = ()
let rec g (x : int) =
  if x <= 0 then
    id
  else
    app g x
let main (u:unit) = g (Random.int 0) ()
