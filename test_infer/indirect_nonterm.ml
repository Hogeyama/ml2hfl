let app h (v : int) (u : unit) = (h v u : unit)
let g (u : unit) = ()
let rec f (x : int) =
  if x <> 0 then
    app f (x-1)
  else
    g
let main () =
  let r = read_int () in
  f r (); assert false
