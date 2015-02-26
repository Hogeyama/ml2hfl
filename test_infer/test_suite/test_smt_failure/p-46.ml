let rec state_0 (a_13:int) (temp0_14:int)= 
  let br_0 = read_int () in
  if br_0>0 then
    if not ((1)>a_13 ) then () else
    let result_11 = temp0_14 in 
    (state_2 (): unit)
  else 
  let br_1 = read_int () in
  if br_1>0 then
    let a_21 = read_int () in 
    if not ((1)<=a_13 ) then () else
    let nondet_12 = read_int () in 
    let x_15 = nondet_12 in 
    let nondet_12 = read_int () in 
    if not (a_13<>x_15*(2) ) then () else
    let a_13 = (1)+a_13*(3) in 
    if not (a_13=(1)+a_21*(3) ) then () else
    if not (a_21<>x_15*(2) ) then () else
    if not ((1)<=a_21 ) then () else
    (state_0 (a_13:int) (temp0_14:int): unit)
  else 
    if not ((1)<=a_13 ) then () else
    let nondet_12 = read_int () in 
    let x_15 = nondet_12 in 
    let nondet_12 = read_int () in 
    if not (a_13=x_15*(2) ) then () else
    let a_13 = x_15 in 
    if not (a_13=x_15 ) then () else
    if not ((1)<=x_15*(2) ) then () else
    (state_0 (a_13:int) (temp0_14:int): unit)
and state_1 (a_13:int) (temp0_14:int) = 
    (state_0 (a_13:int) (temp0_14:int): unit)
and state_2 (u:unit) = 
  ()
let main () =
  let a_13 = read_int () in
  let temp0_14 = read_int () in
  state_1 (a_13:int) (temp0_14:int)
