let rec state_1 (u:unit)= 
    let result_4 = read_int () in 
    (state_300 (): unit)
and state_2 (k_6:int) (x_5:int) = 
  if ((0)<=(-1)-x_5) then
    (state_100 (k_6:int) (x_5:int): unit)
  else
    (state_200 (k_6:int) (x_5:int): unit)

and state_3 (k_6:int) (x_5:int) = 
    (state_2 (k_6:int) (x_5:int): unit)
and state_100 (k_6:int) (x_5:int) = 
    let rho_1 = read_int () in 
    (state_110 (k_6:int) (rho_1:int) (x_5:int): unit)
and state_110 (k_6:int) (rho_1:int) (x_5:int) = 
  let br_0 = read_int () in
  if br_0>0 then
    if not (rho_1<=(1) ) then () else
    (state_111 (k_6:int) (x_5:int): unit)
  else 
  let br_1 = read_int () in
  if br_1>0 then
    if not (rho_1=(2) ) then () else
    (state_112 (k_6:int) (x_5:int): unit)
  else 
  let br_2 = read_int () in
  if br_2>0 then
    if not (rho_1=(3) ) then () else
    (state_113 (k_6:int) (x_5:int): unit)
  else 
    if not (rho_1>=(4) ) then () else
    (state_114 (x_5:int): unit)
and state_111 (k_6:int) (x_5:int) = 
    if not ((0)<=(-1)-x_5 ) then () else
    let x_5 = read_int () in 
    let x_5 = (-1)+x_5 in 
    if not ((0)<=(-1)-x_5 ) then () else
    if not ((0)<=(-1)+k_6 ) then () else
    let x_5 = k_6 in 
    let x_5 = read_int () in 
    let x_5 = (-1)+x_5 in 
    (state_2 (k_6:int) (x_5:int): unit)
and state_112 (k_6:int) (x_5:int) = 
    if not ((0)<=(-1)-x_5 ) then () else
    let x_5 = read_int () in 
    let x_5 = (-1)+x_5 in 
    if not ((0)<=(-1)-x_5 ) then () else
    if not ((-1)+k_6<(0) ) then () else
    let x_5 = read_int () in 
    let x_5 = (-1)+x_5 in 
    (state_2 (k_6:int) (x_5:int): unit)
and state_113 (k_6:int) (x_5:int) = 
    if not ((0)<=(-1)-x_5 ) then () else
    let x_5 = read_int () in 
    let x_5 = (-1)+x_5 in 
    if not ((-1)-x_5<(0) ) then () else
    if not ((0)<=(-1)+x_5 ) then () else
    let x_5 = read_int () in 
    let x_5 = (1)+x_5 in 
    (state_2 (k_6:int) (x_5:int): unit)
and state_114 (x_5:int) = 
    if not ((0)<=(-1)-x_5 ) then () else
    let x_5 = read_int () in 
    let x_5 = (-1)+x_5 in 
    if not ((-1)-x_5<(0) ) then () else
    if not ((-1)+x_5<(0) ) then () else
    (state_1 (): unit)
and state_200 (k_6:int) (x_5:int) = 
    let rho_2 = read_int () in 
    (state_210 (k_6:int) (rho_2:int) (x_5:int): unit)
and state_210 (k_6:int) (rho_2:int) (x_5:int) = 
  let br_0 = read_int () in
  if br_0>0 then
    if not (rho_2<=(1) ) then () else
    (state_211 (k_6:int) (x_5:int): unit)
  else 
  let br_1 = read_int () in
  if br_1>0 then
    if not (rho_2=(2) ) then () else
    (state_212 (k_6:int) (x_5:int): unit)
  else 
  let br_2 = read_int () in
  if br_2>0 then
    if not (rho_2=(3) ) then () else
    (state_213 (k_6:int) (x_5:int): unit)
  else 
  let br_3 = read_int () in
  if br_3>0 then
    if not (rho_2=(4) ) then () else
    (state_214 (x_5:int): unit)
  else 
    if not (rho_2>=(5) ) then () else
    (state_215 (x_5:int): unit)
and state_211 (k_6:int) (x_5:int) = 
    if not ((-1)-x_5<(0) ) then () else
    if not ((0)<=(-1)+x_5 ) then () else
    let x_5 = read_int () in 
    let x_5 = (1)+x_5 in 
    if not ((0)<=(-1)-x_5 ) then () else
    if not ((0)<=(-1)+k_6 ) then () else
    let x_5 = k_6 in 
    let x_5 = read_int () in 
    let x_5 = (-1)+x_5 in 
    (state_2 (k_6:int) (x_5:int): unit)
and state_212 (k_6:int) (x_5:int) = 
    if not ((-1)-x_5<(0) ) then () else
    if not ((0)<=(-1)+x_5 ) then () else
    let x_5 = read_int () in 
    let x_5 = (1)+x_5 in 
    if not ((0)<=(-1)-x_5 ) then () else
    if not ((-1)+k_6<(0) ) then () else
    let x_5 = read_int () in 
    let x_5 = (-1)+x_5 in 
    (state_2 (k_6:int) (x_5:int): unit)
and state_213 (k_6:int) (x_5:int) = 
    if not ((-1)-x_5<(0) ) then () else
    if not ((0)<=(-1)+x_5 ) then () else
    let x_5 = read_int () in 
    let x_5 = (1)+x_5 in 
    if not ((-1)-x_5<(0) ) then () else
    if not ((0)<=(-1)+x_5 ) then () else
    let x_5 = read_int () in 
    let x_5 = (1)+x_5 in 
    (state_2 (k_6:int) (x_5:int): unit)
and state_214 (x_5:int) = 
    if not ((-1)-x_5<(0) ) then () else
    if not ((0)<=(-1)+x_5 ) then () else
    let x_5 = read_int () in 
    let x_5 = (1)+x_5 in 
    if not ((-1)-x_5<(0) ) then () else
    if not ((-1)+x_5<(0) ) then () else
    (state_1 (): unit)
and state_215 (x_5:int) = 
    if not ((-1)-x_5<(0) ) then () else
    if not ((-1)+x_5<(0) ) then () else
    (state_1 (): unit)
and state_300 (u:unit) = 
  ()
let main () =
  let k_6 = read_int () in
  let x_5 = read_int () in
  state_3 (k_6:int) (x_5:int)
