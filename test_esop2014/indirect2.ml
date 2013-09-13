let app (h : unit -> unit) (u : unit) = (h u : unit)
let id (u : unit) = u
let rec f (g:unit -> unit) (x : int) =
  if x > 0 then
    f (app g) (x-1)
  else
    g
let main (n:int) = f id n ()
