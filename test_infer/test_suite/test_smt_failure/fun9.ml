let rec state_2 (u:unit)= 
    let e = (1) in 
    let f = (0) in 
    let b = read_int () in 
    let x = read_int () in 
    if not (x>(0) ) then () else
    (state_3 (b:int) (e:int) (x:int): unit)
and state_3 (b:int) (e:int) (x:int) = 
    let olde = e in 
    (state_21 (b:int) (olde:int) (x:int): unit)
and state_4 (b:int) (e:int) (x:int) = 
  if (x<=(0)) then
    (state_14 (): unit)
  else
    let x = x-(1) in 
    (state_5 (b:int) (e:int) (x:int): unit)

and state_5 (b:int) (e:int) (x:int) = 
    let olde = e in 
    (state_31 (b:int) (olde:int) (x:int): unit)
and state_6 (b:int) (e:int) (x:int) = 
  if (e<=(0)) then
    (state_7 (b:int) (e:int) (x:int): unit)
  else
    let e = e-(1) in 
    (state_9 (b:int) (e:int) (x:int): unit)

and state_7 (b:int) (e:int) (x:int) = 
    let olde = e in 
    (state_41 (b:int) (olde:int) (x:int): unit)
and state_8 (b:int) (e:int) (x:int) = 
    (state_6 (b:int) (e:int) (x:int): unit)
and state_9 (b:int) (e:int) (x:int) = 
    let olde = e in 
    (state_51 (b:int) (olde:int) (x:int): unit)
and state_10 (b:int) (e:int) (x:int) = 
    let olde = e in 
    (state_61 (b:int) (olde:int) (x:int): unit)
and state_11 (b:int) (e:int) (f:int) (x:int) = 
    let f = f+(1) in 
    (state_12 (b:int) (e:int) (x:int): unit)
and state_12 (b:int) (e:int) (x:int) = 
    let olde = e in 
    (state_71 (b:int) (olde:int) (x:int): unit)
and state_13 (b:int) (e:int) (x:int) = 
  let br_0 = read_int () in
  if br_0>0 then
    (state_4 (b:int) (e:int) (x:int): unit)
  else 
    if not (b>x ) then () else
    (state_400 (): unit)
and state_21 (b:int) (olde:int) (x:int) = 
  if (b<=(0)) then
    (state_22 (b:int) (olde:int) (x:int): unit)
  else
    let q = (0) in 
    (state_22 (b:int) (olde:int) (x:int): unit)

and state_22 (b:int) (olde:int) (x:int) = 
    let e = read_int () in 
    let f = read_int () in 
    if not (e>=olde ) then () else
    (state_23 (b:int) (e:int) (olde:int) (x:int): unit)
and state_23 (b:int) (e:int) (olde:int) (x:int) = 
  if (e<=olde) then
    (state_24 (b:int) (e:int) (x:int): unit)
  else
    let q = (1) in 
    (state_24 (b:int) (e:int) (x:int): unit)

and state_24 (b:int) (e:int) (x:int) = 
    (state_4 (b:int) (e:int) (x:int): unit)
and state_31 (b:int) (olde:int) (x:int) = 
  if (b<=(0)) then
    (state_32 (b:int) (olde:int) (x:int): unit)
  else
    let q = (0) in 
    (state_32 (b:int) (olde:int) (x:int): unit)

and state_32 (b:int) (olde:int) (x:int) = 
    let e = read_int () in 
    let f = read_int () in 
    if not (e>=olde ) then () else
    (state_33 (b:int) (e:int) (olde:int) (x:int): unit)
and state_33 (b:int) (e:int) (olde:int) (x:int) = 
  if (e<=olde) then
    (state_34 (b:int) (e:int) (x:int): unit)
  else
    let q = (1) in 
    (state_34 (b:int) (e:int) (x:int): unit)

and state_34 (b:int) (e:int) (x:int) = 
    (state_6 (b:int) (e:int) (x:int): unit)
and state_41 (b:int) (olde:int) (x:int) = 
  if (b<=(0)) then
    (state_42 (b:int) (olde:int) (x:int): unit)
  else
    let q = (0) in 
    (state_42 (b:int) (olde:int) (x:int): unit)

and state_42 (b:int) (olde:int) (x:int) = 
    let e = read_int () in 
    let f = read_int () in 
    if not (e>=olde ) then () else
    (state_43 (b:int) (e:int) (olde:int) (x:int): unit)
and state_43 (b:int) (e:int) (olde:int) (x:int) = 
  let br_0 = read_int () in
  if br_0>0 then
    if not (e=olde ) then () else
    (state_44 (b:int) (e:int) (x:int): unit)
  else 
    if not (e>olde ) then () else
    let q = (1) in 
    (state_44 (b:int) (e:int) (x:int): unit)
and state_44 (b:int) (e:int) (x:int) = 
    (state_8 (b:int) (e:int) (x:int): unit)
and state_51 (b:int) (olde:int) (x:int) = 
  if (b<=(0)) then
    (state_52 (olde:int) (x:int): unit)
  else
    let q = (0) in 
    (state_52 (olde:int) (x:int): unit)

and state_52 (olde:int) (x:int) = 
    let e = read_int () in 
    let f = read_int () in 
    if not (e>=olde ) then () else
    (state_53 (e:int) (olde:int) (x:int): unit)
and state_53 (e:int) (olde:int) (x:int) = 
  if (e<=olde) then
    (state_54 (e:int) (x:int): unit)
  else
    let q = (1) in 
    (state_54 (e:int) (x:int): unit)

and state_54 (e:int) (x:int) = 
    let b = x in 
    (state_10 (b:int) (e:int) (x:int): unit)
and state_61 (b:int) (olde:int) (x:int) = 
  if (b<=(0)) then
    (state_62 (b:int) (olde:int) (x:int): unit)
  else
    let q = (0) in 
    (state_62 (b:int) (olde:int) (x:int): unit)

and state_62 (b:int) (olde:int) (x:int) = 
    let e = read_int () in 
    let f = read_int () in 
    if not (e>=olde ) then () else
    (state_63 (b:int) (e:int) (f:int) (olde:int) (x:int): unit)
and state_63 (b:int) (e:int) (f:int) (olde:int) (x:int) = 
  if (e<=olde) then
    (state_64 (b:int) (e:int) (f:int) (x:int): unit)
  else
    let q = (1) in 
    (state_64 (b:int) (e:int) (f:int) (x:int): unit)

and state_64 (b:int) (e:int) (f:int) (x:int) = 
    (state_11 (b:int) (e:int) (f:int) (x:int): unit)
and state_71 (b:int) (olde:int) (x:int) = 
  if (b<=(0)) then
    (state_72 (b:int) (olde:int) (x:int): unit)
  else
    let q = (0) in 
    (state_72 (b:int) (olde:int) (x:int): unit)

and state_72 (b:int) (olde:int) (x:int) = 
    let e = read_int () in 
    let f = read_int () in 
    if not (e>=olde ) then () else
    (state_73 (b:int) (e:int) (olde:int) (x:int): unit)
and state_73 (b:int) (e:int) (olde:int) (x:int) = 
  if (e<=olde) then
    (state_74 (b:int) (e:int) (x:int): unit)
  else
    let q = (1) in 
    (state_74 (b:int) (e:int) (x:int): unit)

and state_74 (b:int) (e:int) (x:int) = 
    (state_13 (b:int) (e:int) (x:int): unit)
and state_14 (u:unit) = 
  ()
and state_400 (u:unit) = 
  ()
let main () =
  state_2 ()
