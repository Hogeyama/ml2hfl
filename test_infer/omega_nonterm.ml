let ign (x:int) = ()
let rec omega (x:int) = (omega x:unit)
let f (x:int -> unit) (y:int -> unit) (z:int) =
  if z>0 then y z else x z
let main () =
  let r = read_int () in
  f (f omega ign) ign r; assert false
