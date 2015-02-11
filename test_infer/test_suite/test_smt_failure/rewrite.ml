let rec state_0 (u:unit)= 
    (state_1 (): unit)
and state_1 (u:unit) = 
    let x_next = read_int () in 
    let x = x_next in 
    let x_next = read_int () in 
    if not (x+(1)=x_next ) then () else
    let x = x_next in 
    (state_1 (): unit)
let main () =
  state_0 ()
